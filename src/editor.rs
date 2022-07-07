extern crate libc;

use sscanf::scanf;
use std::cmp;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Error, ErrorKind, Read, Result, Write};
use std::slice;

use termios::*;

use libc::{ioctl, winsize, STDOUT_FILENO, TIOCGWINSZ};

#[derive(Copy, Clone)]
enum EKey {
    Escape = '\x1b' as isize, // 27
    ArrowLeft = 1000,
    ArrowRight,
    ArrowUp,
    ArrowDown,
    DelKey,
    HomeKey,
    EndKey,
    PageUp,
    PageDown,
}

const EAGAIN: i32 = 11;
const CARP_VER: &str = "0.0.1";

fn enable_raw_mode() -> Termios {
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

fn disable_raw_mode(orig_term: &Termios) {
    // sending rmcup command to disable making Page UP / DOWN work macOS
    io::stdout().write(b"\033[?47l").unwrap();
    // reset cursor position, this seems to prevent weird whitespace at top of terminal after exiting
    io::stdout().write(b"\x1b[H").unwrap();
    io::stdout().flush().unwrap();

    if let Err(err) = tcsetattr(0, TCSAFLUSH, orig_term) {
        die("tcsetattr", &err.to_string());
    }
}

// note: diverging function, returns !
fn die(msg: &str, error: &str) -> ! {
    io::stdout().write(b"\x1b[2J").unwrap();
    io::stdout().write(b"\x1b[H").unwrap();
    io::stdout().flush().unwrap();
    eprintln!("{}: {}", msg, error);
    panic!();
}

pub struct Editor {
    cx: usize,
    cy: usize,
    screen_rows: usize,
    screen_cols: usize,
    orig_term: Termios,
    erows: Vec<String>,
    nrows: usize,
}

impl Editor {
    pub fn new(filename_opt: Option<&String>) -> Self {
        let orig_term = enable_raw_mode();

        let (screen_rows, screen_cols) = Editor::get_win_size().unwrap_or_else(|err| {
            die("getWindowSize", &err.to_string());
        });

        let (nrows, erows) = Editor::open_file(filename_opt).unwrap_or_else(|err| {
            die("fopen", &err.to_string());
        });

        Self {
            cx: 0,
            cy: 0,
            screen_rows,
            screen_cols,
            orig_term,
            erows,
            nrows,
        }
    }

    pub fn open_file(filename_opt: Option<&String>) -> Result<(usize, Vec<String>)> {
        // read file
        let mut nrows: usize = 0;
        let mut erows = Vec::new();
        if let Some(filename) = filename_opt {
            let file = File::open(filename)?;
            // NOTE: Each line (string) from lines() will not have \n or \r at the end
            // TODO: optimize by reusing buffer for buffer reader. See https://stackoverflow.com/questions/45882329/read-large-files-line-by-line-in-rust
            for line in BufReader::new(file).lines() {
                if let Ok(ip) = line {
                    // we need to append this to the erows
                    erows.push(ip);
                    nrows += 1;
                }
            }
        }

        return Ok((nrows, erows));
    }

    pub fn run(&mut self) {
        loop {
            self.refresh_screen();
            self.process_keypress();
        }
    }

    fn draw_rows(&self, buf: &mut String) {
        for y in 0..self.screen_rows {
            if y >= self.nrows {
                // beyond the number of lines in the opened file
                if self.nrows == 0 && y == self.screen_rows / 3 {
                    let welcome_msg = format!("carp editor -- version {}", CARP_VER);
                    let screen_len = cmp::min(self.screen_cols, welcome_msg.len());
                    let mut padding = (self.screen_cols - screen_len) / 2;

                    if padding > 0 {
                        buf.push_str("~");
                        padding -= 1;
                    }

                    while padding > 0 {
                        buf.push_str(" ");
                        padding -= 1;
                    }

                    buf.push_str(&welcome_msg[..screen_len]);
                } else {
                    buf.push_str("~");
                }
            } else {
                // append file contents to buffer
                let max_len = cmp::min(self.screen_cols, self.erows[y].len());
                buf.push_str(&self.erows[y][..max_len]);
            }

            // clear line to right of cursor position
            // NOTE: this replaces whole screen clearing with \x1b[2J
            buf.push_str("\x1b[K");

            if y < self.screen_rows - 1 {
                buf.push_str("\r\n");
            }
        }
    }

    fn refresh_screen(&self) {
        let mut buf = String::new();

        // hide cursor before refresh screen
        // NOTE: may not be supported on all terminals
        buf.push_str("\x1b[?25l");

        // reset cursor position at top left
        buf.push_str("\x1b[H");

        self.draw_rows(&mut buf);

        // draw cursor position
        buf.push_str(&format!("\x1b[{};{}H", self.cy + 1, self.cx + 1));

        // show cursor again
        buf.push_str("\x1b[?25h");

        io::stdout().write(buf.as_bytes()).unwrap();
        io::stdout().flush().unwrap();
    }

    fn move_cursor(&mut self, key: u32) {
        if key == EKey::ArrowLeft as u32 {
            if self.cx != 0 {
                self.cx -= 1;
            }
        } else if key == EKey::ArrowDown as u32 {
            if self.cy != self.screen_rows - 1 {
                self.cy += 1;
            }
        } else if key == EKey::ArrowUp as u32 {
            if self.cy != 0 {
                self.cy -= 1;
            }
        } else if key == EKey::ArrowRight as u32 {
            if self.cx != self.screen_cols - 1 {
                self.cx += 1;
            }
        }
    }

    fn process_keypress(&mut self) {
        let key: u32 = Editor::read_key();
        let bits_q: u32 = ('q' as u32) & 0x1f;

        if key == bits_q {
            io::stdout().write(b"\x1b[2J").unwrap();
            io::stdout().write(b"\x1b[H").unwrap();
            io::stdout().flush().unwrap();
            self.cleanup();
            std::process::exit(0);
        }

        let vec_keys2u32 = |v: Vec<EKey>| v.iter().map(|x| *x as u32).collect();

        let page_keys: Vec<u32> = vec_keys2u32(vec![EKey::PageDown, EKey::PageUp]);
        let arrow_keys: Vec<u32> = vec_keys2u32(vec![
            EKey::ArrowUp,
            EKey::ArrowDown,
            EKey::ArrowLeft,
            EKey::ArrowRight,
        ]);

        if page_keys.contains(&key) {
            // note: on macOS, we need to press Shift + Fn + Up/Down arrows to register it as Page Up/Down
            let times = self.screen_rows;
            for _ in 0..times {
                let correct_arrow = if key == (EKey::PageUp as u32) {
                    EKey::ArrowUp
                } else {
                    EKey::ArrowDown
                };
                self.move_cursor(correct_arrow as u32);
            }
        } else if arrow_keys.contains(&key) {
            self.move_cursor(key);
        } else if key == EKey::HomeKey as u32 {
            self.cx = 0;
        } else if key == EKey::EndKey as u32 {
            self.cx = self.screen_cols - 1;
        }
    }

    fn read_key() -> u32 {
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
                eprintln!("second_ch: {}", second_ch);

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
                            '1' | '7' => EKey::HomeKey,
                            '3' => EKey::DelKey,
                            '4' | '8' => EKey::EndKey,
                            '5' => EKey::PageUp,
                            '6' => EKey::PageDown,
                            _ => EKey::Escape,
                        } as u32;
                    }
                } else {
                    return match seq[1] as char {
                        'A' => EKey::ArrowUp,
                        'B' => EKey::ArrowDown,
                        'C' => EKey::ArrowRight,
                        'D' => EKey::ArrowLeft,
                        'H' => EKey::HomeKey,
                        'F' => EKey::EndKey,
                        _ => EKey::Escape,
                    } as u32;
                }
            } else if seq[0] as char == 'O' {
                return match seq[1] as char {
                    'H' => EKey::HomeKey,
                    'F' => EKey::EndKey,
                    _ => EKey::Escape,
                } as u32;
            }

            return '\x1b' as u32;
        }

        return buffer[0] as u32;
    }

    fn get_cursor_pos() -> Result<(usize, usize)> {
        if io::stdout().write(b"\x1b[6n").unwrap() != 4 {
            return Err(Error::last_os_error());
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
            return Err(Error::last_os_error());
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

        return Err(Error::last_os_error());
    }

    fn get_win_size() -> Result<(usize, usize)> {
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
                return Err(Error::last_os_error());
            }
            let res = Editor::get_cursor_pos();
            // if let Ok((r,c)) = res {
            //     eprintln!("rows: {} | cols: {} \r", r, c);
            // }
            return res;
        }

        // eprintln!("rows: {} | cols: {} \r", wsz.ws_row, wsz.ws_col);
        Ok((wsz.ws_row as usize, wsz.ws_col as usize))
    }

    pub fn cleanup(&self) {
        disable_raw_mode(&self.orig_term);
    }
}
