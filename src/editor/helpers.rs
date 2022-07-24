use super::constant::*;

use libc::{ioctl, winsize, STDOUT_FILENO, TIOCGWINSZ};
use sscanf::scanf;
use std::fs::File;
use std::io::{self, BufRead, BufReader, ErrorKind, Read, Write};
use std::path::Path;
use std::slice;
use termios::*;

/** terminal stuff */
pub fn enable_raw_mode() -> Termios {
    let termios = match Termios::from_fd(0) {
        Ok(instance) => instance,
        Err(err) => die("tcgetattr", &err.to_string()),
    };

    let mut raw_termios = termios.clone();
    raw_termios.c_iflag &= !(BRKINT | INPCK | ISTRIP | ICRNL | IXON);
    raw_termios.c_oflag &= !(OPOST);
    raw_termios.c_cflag |= CS8;
    raw_termios.c_lflag &= !(ICANON | ECHO | ISIG | IEXTEN);
    raw_termios.c_cc[VMIN] = 0;
    raw_termios.c_cc[VTIME] = 1; // per 0.1s

    if let Err(err) = tcsetattr(0, TCSAFLUSH, &mut raw_termios) {
        die("tcsetattr", &err.to_string());
    }

    // sending smcup command to enable making Page UP / DOWN work on macOS
    io::stdout().write(b"\033[?47h").unwrap();
    io::stdout().flush().unwrap();

    return termios;
}

pub fn get_win_size() -> io::Result<(usize, usize)> {
    let wsz = winsize {
        ws_row: 0,
        ws_col: 0,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };

    if unsafe { ioctl(STDOUT_FILENO, TIOCGWINSZ, &wsz) } == -1 || wsz.ws_col == 0 {
        // move cursor to bottom right (C = Cursor Forward, B = Cursor Down)
        let n = io::stdout().write(b"\x1b[999C\x1b[999B")?;
        io::stdout().flush()?;
        if n != 12 {
            return Err(io::Error::last_os_error());
        }
        let res = get_cursor_pos();
        // if let Ok((r,c)) = res {
        //     eprintln!("rows: {} | cols: {} \r", r, c);
        // }
        return res;
    }

    // eprintln!("rows: {} | cols: {} \r", wsz.ws_row, wsz.ws_col);
    Ok((wsz.ws_row as usize, wsz.ws_col as usize))
}

pub fn disable_raw_mode(orig_term: &Termios) {
    // sending rmcup command to disable making Page UP / DOWN work macOS
    io::stdout().write(b"\033[?47l").unwrap();
    // reset cursor position, this seems to prevent weird whitespace at top of terminal after exiting
    io::stdout().write(b"\x1b[H").unwrap();
    io::stdout().flush().unwrap();

    if let Err(err) = tcsetattr(0, TCSAFLUSH, orig_term) {
        die("tcsetattr", &err.to_string());
    }
}

pub fn read_key() -> u32 {
    let mut buffer = [0; 1];

    while let Err(err) = io::stdin().read_exact(&mut buffer) {
        if let Some(errno) = err.raw_os_error() {
            if errno != EAGAIN {
                die("read", &err.to_string());
            }
        }
    }

    if buffer[0] as char == '\x1b' {
        let mut seq = [0 as u8; 3];

        for i in 0..2 {
            if io::stdin()
                .read_exact(slice::from_mut(&mut seq[i]))
                .is_err()
            {
                return '\x1b' as u32;
            }
        }

        if seq[0] as char == '[' {
            let second_ch = seq[1] as char;

            if second_ch >= '0' && second_ch <= '9' {
                if io::stdin()
                    .read_exact(slice::from_mut(&mut seq[2]))
                    .is_err()
                {
                    return '\x1b' as u32;
                }

                let third_ch = seq[2] as char;

                if third_ch == '~' {
                    return match second_ch {
                        '1' | '7' => EKey::HOME_KEY,
                        '3' => EKey::DEL_KEY,
                        '4' | '8' => EKey::END_KEY,
                        '5' => EKey::PAGE_UP,
                        '6' => EKey::PAGE_DOWN,
                        _ => EKey::ESCAPE,
                    } as u32;
                }
            } else {
                return match seq[1] as char {
                    'A' => EKey::ARROW_UP,
                    'B' => EKey::ARROW_DOWN,
                    'C' => EKey::ARROW_RIGHT,
                    'D' => EKey::ARROW_LEFT,
                    'H' => EKey::HOME_KEY,
                    'F' => EKey::END_KEY,
                    _ => EKey::ESCAPE,
                } as u32;
            }
        } else if seq[0] as char == 'O' {
            return match seq[1] as char {
                'H' => EKey::HOME_KEY,
                'F' => EKey::END_KEY,
                _ => EKey::ESCAPE,
            } as u32;
        }

        return '\x1b' as u32;
    }

    return buffer[0] as u32;
}

// note: diverging function, returns !
pub fn die(msg: &str, error: &str) -> ! {
    io::stdout().write(b"\x1b[2J").unwrap();
    io::stdout().write(b"\x1b[H").unwrap();
    io::stdout().flush().unwrap();
    eprintln!("{}: {}", msg, error);
    panic!();
}

pub fn get_cursor_pos() -> io::Result<(usize, usize)> {
    if io::stdout().write(b"\x1b[6n").unwrap() != 4 {
        return Err(io::Error::last_os_error());
    }

    // NOTE: we need to flush stdout
    io::stdout().flush()?;

    let mut buffer = [0 as u8; 32];
    let mut i = 0;

    // buffer shoud be 32 bytes
    while i < 31 {
        // read a single byte from stdin
        let res = io::stdin().read_exact(slice::from_mut(&mut buffer[i]));

        if let Err(_) = res {
            break;
        }

        // print!("i: {} | {} \r\n", i, buffer[i] as char);

        if (buffer[i] as char) == 'R' {
            break;
        }

        i += 1;
    }

    // print!("\r\n&buf[1]: '{:?}'\r\n", &buffer[1..32]);
    if (buffer[0] as char) != '\x1b' || (buffer[1] as char) != '[' {
        return Err(io::Error::last_os_error());
    }

    // NOTE: Rust doesn't seem to use NULL character to demarcate end of string, like:
    // buffer[i] = 0x00; // equivalent to '\0' NULL character
    // For now, we trim it, but maybe in future we can use Vec::with_capacity()
    let trim_buf = &buffer[2..i];

    // TODO: handle this better as we are now just mapping the error to ErrorKind::InvalidInput to satisfy IO Error type
    let buf_str: &str = std::str::from_utf8(&trim_buf).map_err(|_| ErrorKind::InvalidInput)?;

    let parsed = scanf!(buf_str, "{};{}", u16, u16);

    if let Ok((row, col)) = parsed {
        return Ok((row as usize, col as usize));
    }

    return Err(io::Error::last_os_error());
}

/** helper functions */
pub fn read_file<P>(filename_opt: Option<&P>) -> io::Result<Vec<String>>
where
    P: AsRef<Path>,
{
    let mut erows: Vec<String> = Vec::new();

    if let Some(filename) = filename_opt {
        let file = File::open(filename)?;
        // NOTE: Each line (string) from lines() will not have \n or \r at the end
        // TODO: optimize by reusing buffer for buffer reader. See https://stackoverflow.com/questions/45882329/read-large-files-line-by-line-in-rust
        for line in BufReader::new(file).lines() {
            if let Ok(ip) = line {
                // we need to append this to the erows
                erows.push(ip);
            }
        }
    }

    return Ok(erows);
}

pub fn rx_to_cx(erow: &str, target: usize) -> usize {
    if target == 0 {
        return 0;
    }

    let mut rx = 0;

    for (idx, c) in erow.chars().enumerate() {
        if c == '\t' {
            rx += (TAB_WIDTH - 1) - (rx % TAB_WIDTH);
        }
        rx += 1;

        if rx == target {
            return idx + 1;
        }
    }

    unreachable!("Should always return a valid cx!");
}

pub fn cx_to_rx(erow: &str, cx: usize) -> usize {
    let mut rx = 0;

    for i in 0..cx {
        if (erow.as_bytes()[i] as char) == '\t' {
            rx += (TAB_WIDTH - 1) - (rx % TAB_WIDTH);
        }
        rx += 1;
    }

    return rx;
}

pub fn is_separator(c: char) -> bool {
    c.is_whitespace() || c == '\0' || ",.()+-/*=~%<>[];".contains(c)
}
