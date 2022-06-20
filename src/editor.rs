extern crate libc;

use std::io;
use std::slice;
use std::ops::Drop;
use std::io::{Read, Write, Result, Error, ErrorKind};
use sscanf::scanf;

use termios::*;

use libc::{ioctl, winsize, TIOCGWINSZ, STDOUT_FILENO, atexit};

const EAGAIN: i32 = 11;

pub struct Editor {
    screenRows: u16,
    screenCols: u16,
    orig_term: Termios,
}

fn enable_raw_mode() -> Termios {
    let termios = match Termios::from_fd(0) {
        Ok(instance) => instance,
        Err(err) => die("tcgetattr", &err.to_string()),
    };

    let mut raw_termios = termios.clone(); 
    raw_termios.c_iflag &= !(BRKINT | INPCK | ISTRIP | ICRNL | IXON);
    raw_termios.c_oflag &= !(OPOST);
    raw_termios.c_cflag |= (CS8);
    raw_termios.c_lflag &= !(ICANON | ECHO | ISIG | IEXTEN);
    raw_termios.c_cc[VMIN]  = 0;
    raw_termios.c_cc[VTIME] = 1;

    if let Err(err) = tcsetattr(0, TCSAFLUSH, &mut raw_termios) {
        die("tcsetattr", &err.to_string());
    }

    return termios;
}

fn disable_raw_mode(orig_term: &Termios) {
    if let Err(err) = tcsetattr(0, TCSAFLUSH, orig_term) {
        die("tcsetattr", &err.to_string());
    }
}

fn is_control(c: u8) -> bool {
    return (c >= 0 && c <= 31) || c == 127;
}

fn CTRL_KEY(c: char) -> u8 {
    return (c as u8) & 0x1f;
}

// note: diverging function, returns !
fn die(msg: &str, error: &str) -> ! {
    // io::stdout().write(b"\x1b[2J").unwrap();
    // io::stdout().write(b"\x1b[H").unwrap();
    eprintln!("{}: {}", msg, error);
    panic!();
}


impl Editor {
    pub fn new() -> Self {
        let orig_term = enable_raw_mode();
        
        let (rows, cols) = Editor::get_win_size().unwrap_or_else(|err| {
            die("getWindowSize", &err.to_string());
        });

        Self {
            screenRows: rows,
            screenCols: cols,
            orig_term
        }
    }

    pub fn run(&self) {
        loop {
            self.refresh_screen();
            self.process_keypress();
        }
    }

    fn refresh_screen(&self) {
        // clear screen
        let wbytes: usize = io::stdout().write(b"\x1b[2J").unwrap();
        assert!(wbytes == 4);

        // reset cursor position at top left
        io::stdout().write(b"\x1b[H").unwrap();

        self.draw_rows();

        io::stdout().write(b"\x1b[H").unwrap();
    }

    fn process_keypress(&self) {
        let c: u8 = Editor::read_key();
        let bits_q: u8 = CTRL_KEY('q');

        if c == bits_q {
            io::stdout().write(b"\x1b[2J").unwrap();
            io::stdout().write(b"\x1b[H").unwrap();
            self.cleanup();
            std::process::exit(0);
        } else {
            // TODO
        }
    }

    fn draw_rows(&self) {
        for y in 0..self.screenRows {
            io::stdout().write(b"~\r\n");
        }
    }

    fn read_key() -> u8 {
        let mut buffer = [0;1];

        if let Err(err) = io::stdin().read_exact(&mut buffer) {
            if let Some(errno) = err.raw_os_error() {
                if errno != EAGAIN {
                    die("read", &err.to_string());
                }
            }
        }

        buffer[0]
    }

    fn get_cursor_pos() -> Result<(u16,u16)>  {

        if io::stdout().write(b"\x1b[6n").unwrap() != 4 {
            return Err(Error::last_os_error());
        }

        // NOTE: we need to flush stdout
        io::stdout().flush();

        let mut buffer = [0 as u8;32];
        let mut i = 0;

        // buffer shoud be 32 bytes
        while i < 31 {
            // read a single byte from stdin
            let res = io::stdin().read_exact( slice::from_mut(&mut buffer[i]) );

            if let Err(err) = res { 
                break;
            }

            // print!("i: {} | {} \r\n", i, buffer[i] as char);

            if (buffer[i] as char) == 'R' { break; }

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

        let buf_str: &str = std::str::from_utf8(&trim_buf).map_err(|_| ErrorKind::InvalidInput)?;
        
        let parsed = scanf!(buf_str, "{};{}", u16, u16);

        if let Ok((row, col)) = parsed {
            return Ok((row, col));
        } 

        return Err(Error::last_os_error());
    }

    fn get_win_size() -> Result<(u16,u16)> {
        let wsz = winsize {
            ws_row: 0,
            ws_col: 0,
            ws_xpixel: 0,
            ws_ypixel: 0,
        };

        if unsafe { ioctl(STDOUT_FILENO, TIOCGWINSZ, &wsz) } == -1 || wsz.ws_col == 0 {
            // move cursor to bottom right (C = Cursor Forward, B = Cursor Down)
            let n = io::stdout().write(b"\x1b[999C\x1b[999B").unwrap();
            if n != 12 { return Err(Error::last_os_error()); }
            let res = Editor::get_cursor_pos();
            // if let Ok((r,c)) = res {
            //     eprintln!("rows: {} | cols: {} \r", r, c);
            // }
            return res;
        }

        // eprintln!("rows: {} | cols: {} \r", wsz.ws_row, wsz.ws_col);
        Ok((wsz.ws_row, wsz.ws_col))
    }

    pub fn cleanup(&self) {
        disable_raw_mode(&self.orig_term);
    }
}
