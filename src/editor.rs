extern crate libc;

use sscanf::scanf;
use std::cmp;
use std::fmt;
use std::fs::File;
use std::io::{self, BufRead, BufReader, ErrorKind, Read, Write};
use std::path::Path;
use std::slice::{self};
use std::time::Instant;

use termios::*;

use libc::{ioctl, winsize, STDOUT_FILENO, TIOCGWINSZ};

#[non_exhaustive]
struct EKey;

impl EKey {
    pub const ESCAPE: u32 = '\x1b' as u32;
    pub const BACKSPACE: u32 = 127;
    pub const ARROW_LEFT: u32 = 1000;
    pub const ARROW_RIGHT: u32 = 1001;
    pub const ARROW_UP: u32 = 1002;
    pub const ARROW_DOWN: u32 = 1003;
    pub const DEL_KEY: u32 = 1004;
    pub const HOME_KEY: u32 = 1005;
    pub const END_KEY: u32 = 1006;
    pub const PAGE_UP: u32 = 1007;
    pub const PAGE_DOWN: u32 = 1008;
}

const EAGAIN: i32 = 11;
const CARP_VER: &str = "0.0.1";
const TAB_WIDTH: usize = 8;
const STATUS_HEIGHT: usize = 2;
const QUIT_TIMES: u8 = 3;
const ENTER: u32 = '\r' as u32;
const CTRL_Q: u32 = CTRL('q');
const CTRL_F: u32 = CTRL('f');
const CTRL_H: u32 = CTRL('h');
const CTRL_L: u32 = CTRL('l');
const CTRL_S: u32 = CTRL('s');

/** custom error for file saving */
enum FileSaveError {
    Io(io::Error),
    NoFileSpecifiedError,
}

impl fmt::Display for FileSaveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FileSaveError::Io(ref cause) => write!(f, "I/O Error: {}", cause),
            FileSaveError::NoFileSpecifiedError => write!(f, "No file specified for saving!"),
        }
    }
}

impl From<io::Error> for FileSaveError {
    fn from(cause: io::Error) -> FileSaveError {
        FileSaveError::Io(cause)
    }
}

/** terminal stuff */
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

fn get_win_size() -> io::Result<(usize, usize)> {
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
fn die(msg: &str, error: &str) -> ! {
    io::stdout().write(b"\x1b[2J").unwrap();
    io::stdout().write(b"\x1b[H").unwrap();
    io::stdout().flush().unwrap();
    eprintln!("{}: {}", msg, error);
    panic!();
}

fn get_cursor_pos() -> io::Result<(usize, usize)> {
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
fn read_file<P>(filename_opt: Option<&P>) -> io::Result<Vec<Row>>
where
    P: AsRef<Path>,
{
    let mut erows: Vec<Row> = Vec::new();

    if let Some(filename) = filename_opt {
        let file = File::open(filename)?;
        // NOTE: Each line (string) from lines() will not have \n or \r at the end
        // TODO: optimize by reusing buffer for buffer reader. See https://stackoverflow.com/questions/45882329/read-large-files-line-by-line-in-rust
        for line in BufReader::new(file).lines() {
            if let Ok(ip) = line {
                // we need to append this to the erows
                erows.push(Row::new(&ip));
            }
        }
    }

    return Ok(erows);
}

const fn CTRL(c: char) -> u32 {
    (c as u32) & 0x1f
}

/** row helper stuff */
fn rx_to_cx(erow: &str, target: usize) -> usize {
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

fn cx_to_rx(erow: &str, cx: usize) -> usize {
    let mut rx = 0;

    for i in 0..cx {
        if (erow.as_bytes()[i] as char) == '\t' {
            rx += (TAB_WIDTH - 1) - (rx % TAB_WIDTH);
        }
        rx += 1;
    }

    return rx;
}

/** highlights */

#[derive(Clone, Copy, PartialEq)]
enum HLType {
    Normal = 0,
    Number,
    Match,
}

impl HLType {
    fn to_ansi_color(&self) -> u8 {
        match self {
            HLType::Number => 31, // red
            HLType::Match => 34,  // blue
            _ => 37,
        }
    }
}

struct Row {
    content: String,
    render: String,
    hl: Vec<HLType>, // 0 - 255 number codes
}

impl Row {
    pub fn new<T: Into<String>>(ip: T) -> Row {
        // NOTE: <ip> var is moved due to into() and is not usable after the next line
        let content = ip.into();
        let render = Row::content_to_rendered(&content);
        let hl = Row::rendered_to_hl(&render);

        Row {
            content,
            render,
            hl,
        }
    }

    fn update_render(&mut self) {
        self.render = Row::content_to_rendered(&self.content);
        self.hl = Row::rendered_to_hl(&self.render);
    }

    fn replace_content<T: Into<String>>(&mut self, s: T) {
        self.content = s.into();
        self.update_render();
    }

    fn del_char(&mut self, at: usize) {
        if at >= self.content.len() {
            return; // prevent panic! for remove() call below
        }
        self.content.remove(at);
        self.update_render();
    }

    fn insert_char(&mut self, at: usize, c: char) {
        self.content.insert(cmp::min(at, self.content.len()), c);
        self.update_render();
    }

    fn append_str(&mut self, s: &str) {
        self.content.push_str(s);
        self.update_render();
    }

    fn is_digit_separator(c: char) -> bool {
        c.is_whitespace() || c == '\0' || ",.()+-/*=~%<>[];".contains(c)
    }

    fn rendered_to_hl(render: &String) -> Vec<HLType> {
        let mut hl = vec![HLType::Normal; render.len()];

        let mut i = 0;

        while i < render.len() {
            // TODO: Support unicode properly as .len() above on string returns no. of bytes not characters
            let c = render.as_bytes()[i] as char;
            let prev_char_sep = i == 0 || Row::is_digit_separator(render.as_bytes()[i - 1] as char);

            if (c.is_ascii_digit() && (prev_char_sep || hl[i - 1] == HLType::Number))
                || (c == '.' && hl[i - 1] == HLType::Number)
            {
                hl[i] = HLType::Number;
            }

            i += 1;
        }

        return hl;
    }

    fn content_to_rendered(line: &String) -> String {
        // replace all tab characters in the line with appropriate number of spaces
        let mut new_line = String::new();
        let mut tabs = 0;
        for ch in line.chars() {
            if ch == '\t' {
                new_line.push(' ');
                if (tabs + 1) % TAB_WIDTH != 0 {
                    let nspaces = TAB_WIDTH - ((tabs + 1) % TAB_WIDTH);
                    let tab_spaces = " ".repeat(nspaces);
                    new_line.push_str(&tab_spaces);
                }
                tabs = 0;
            } else {
                new_line.push(ch);
                tabs += 1;
            }
        }
        return new_line;
    }
}

pub struct Editor {
    cx: usize,
    rx: usize, // render index
    cy: usize,
    screen_rows: usize,
    screen_cols: usize,
    orig_term: Termios,
    erows: Vec<Row>,
    roffset: usize,
    coffset: usize,
    filename: Option<String>,
    status_msg: Option<String>,
    status_time: Instant,
    dirty: bool,
}

impl Editor {
    pub fn new(filename_opt: Option<&String>) -> Self {
        let orig_term = enable_raw_mode();

        let (screen_rows, screen_cols) = get_win_size().unwrap_or_else(|err| {
            die("getWindowSize", &err.to_string());
        });

        let erows = read_file(filename_opt).unwrap_or_else(|err| {
            die("fopen", &err.to_string());
        });

        Self {
            cx: 0,
            rx: 0,
            cy: 0,
            screen_rows: screen_rows - STATUS_HEIGHT,
            screen_cols,
            orig_term,
            erows,
            roffset: 0,
            coffset: 0,
            filename: filename_opt.map(|s| s.to_owned()),
            status_msg: None,
            status_time: Instant::now(),
            dirty: false,
        }
    }

    fn set_status_msg(&mut self, msg: &str) {
        self.status_msg = Some(msg.to_string());
        self.status_time = Instant::now();
    }

    pub fn run(&mut self) {
        self.set_status_msg("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

        loop {
            self.refresh_screen();
            self.process_keypress();
        }
    }

    fn draw_rows(&self, buf: &mut String) {
        for y in 0..self.screen_rows {
            let abs_y = y + self.roffset; // y position in file

            if abs_y >= self.erows.len() {
                // beyond the number of lines in the opened file
                if self.erows.len() == 0 && y == self.screen_rows / 3 {
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
                let line_len = self.erows[abs_y].render.len();
                if self.coffset < line_len {
                    let clipped_len = cmp::min(self.screen_cols, line_len - self.coffset);
                    let mut current_color: i16 = -1; // i16 smallest signed type that allows safe casting from u8
                    let render_row = &self.erows[abs_y].render[self.coffset..];
                    let hl_row = &self.erows[abs_y].hl[self.coffset..];

                    for i in 0..clipped_len {
                        if hl_row[i] == HLType::Normal {
                            if current_color != -1 {
                                buf.push_str("\x1b[39m");
                                current_color = -1;
                            }
                            buf.push(render_row.as_bytes()[i] as char);
                        } else {
                            let color = hl_row[i].to_ansi_color();

                            if current_color != color as i16 {
                                current_color = color as i16;
                                buf.push_str(&format!("\x1b[{}m", color));
                            }
                            buf.push(render_row.as_bytes()[i] as char);
                        }
                    }
                }
                buf.push_str("\x1b[39m");
            }

            // clear line to right of cursor position
            // NOTE: this replaces whole screen clearing with \x1b[2J
            buf.push_str("\x1b[K");

            buf.push_str("\r\n");
        }
    }

    /** erows operations  */
    fn insert_row_at<T>(&mut self, at: usize, row: T)
    where
        T: Into<String>,
    {
        let mut idx = at;
        if at > self.erows.len() {
            idx = self.erows.len();
        }
        self.erows.insert(idx, Row::new(row.into()));
        self.dirty = true;
    }

    fn del_row(&mut self, at: usize) {
        if at >= self.erows.len() {
            return;
        }
        self.erows.remove(at);
        self.dirty = true;
    }

    /** editor operations  */
    fn insert_char(&mut self, c: char) {
        if self.cy == self.erows.len() {
            // one line after the last text, so we start a new empty row
            self.insert_row_at(self.erows.len(), "");
        }

        self.erows[self.cy].insert_char(self.cx, c);
        self.dirty = true;
        self.cx += 1;
    }

    fn del_char(&mut self) {
        if self.cy == self.erows.len() {
            return;
        }

        if self.cx == 0 && self.cy == 0 {
            return;
        }

        if self.cx > 0 {
            let row = &mut self.erows[self.cy];
            row.del_char(self.cx - 1);
            self.cx -= 1;
        } else {
            let curr_row_content = self.erows[self.cy].content.to_owned();
            let prev_row = &mut self.erows[self.cy - 1]; // guaranteed to exist
            self.cx = prev_row.content.len();
            prev_row.append_str(&curr_row_content);
            self.del_row(self.cy);
            self.cy -= 1;
        }
        self.dirty = true;
    }

    fn insert_newline(&mut self) {
        if self.cx == 0 {
            self.insert_row_at(self.cy, "");
        } else {
            let mut cur_row = &mut self.erows[self.cy]; // guaranteed to exist as cx > 0

            // TODO: change all this when handling unicode correctly
            let next_row_str: String = cur_row.content.chars().skip(self.cx).collect();
            self.insert_row_at(self.cy + 1, next_row_str);

            cur_row = &mut self.erows[self.cy];
            let cur_row_new: String = cur_row.content.chars().take(self.cx).collect();
            cur_row.replace_content(cur_row_new);
        }

        self.cy += 1;
        self.cx = 0;
    }

    /** output ops */
    fn scroll_screen(&mut self) {
        self.rx = self.cx;
        if self.cy < self.erows.len() {
            // we are not at the last allowed line of the editor (i.e. one line after end of text)
            self.rx = cx_to_rx(&self.erows[self.cy].content, self.cx);
        }

        // if cursor goes above the top of screen display, change roffset
        if self.cy < self.roffset {
            self.roffset = self.cy;
        }

        // if cursor goes below the bottom of screen, handle accordingly
        if self.cy >= self.roffset + self.screen_rows {
            self.roffset = self.cy - (self.screen_rows - 1);
        }

        // same for horizontal scrolling
        if self.rx < self.coffset {
            self.coffset = self.rx;
        }

        if self.rx >= self.coffset + self.screen_cols {
            self.coffset = self.rx - (self.screen_cols - 1);
        }
    }

    fn draw_status(&self, buf: &mut String) {
        buf.push_str("\x1b[7m");

        // status message
        // NOTE: as_deref makes supplying &str in unwrap_or() possible
        let filename = self.filename.as_deref().unwrap_or("No Name");
        let file_dirty_msg = if self.dirty { "(modified)" } else { "" };
        let status_msg = &format!(
            "{:20} - {} lines {}",
            filename,
            self.erows.len(),
            file_dirty_msg
        );
        let mut len = cmp::min(status_msg.len(), self.screen_cols);
        buf.push_str(&status_msg[..len]);

        let row_stat_msg = format!("{}/{}", self.cy + 1, self.erows.len());

        while len < self.screen_cols {
            if self.screen_cols - len == row_stat_msg.len() {
                buf.push_str(&row_stat_msg);
                break;
            } else {
                buf.push(' ');
                len += 1;
            }
        }

        buf.push_str("\x1b[m");

        // second line below
        buf.push_str("\r\n");
    }

    fn draw_msg(&self, buf: &mut String) {
        // clear line
        buf.push_str("\x1b[K");

        if self.status_msg.is_some() && self.status_time.elapsed().as_secs() < 5 {
            let msg: &str = self.status_msg.as_deref().unwrap();
            buf.push_str(&msg[..cmp::min(msg.len(), self.screen_cols)]);
        }
    }

    fn refresh_screen(&mut self) {
        self.scroll_screen();

        let mut buf = String::new();

        // hide cursor before refresh screen
        // NOTE: may not be supported on all terminals
        buf.push_str("\x1b[?25l");

        // reset cursor position at top left
        buf.push_str("\x1b[H");

        self.draw_rows(&mut buf);
        self.draw_status(&mut buf);
        self.draw_msg(&mut buf);

        // draw cursor position
        buf.push_str(&format!(
            "\x1b[{};{}H",
            (self.cy - self.roffset) + 1,
            (self.rx - self.coffset) + 1,
        ));

        // show cursor again
        buf.push_str("\x1b[?25h");

        io::stdout().write(buf.as_bytes()).unwrap();
        io::stdout().flush().unwrap();
    }

    /** input */
    fn move_cursor(&mut self, key: u32) {
        let row = self.erows.get(self.cy);

        if key == EKey::ARROW_LEFT {
            if self.cx > 0 {
                self.cx -= 1;
            } else if self.cy > 0 {
                self.cy -= 1;
                self.cx = self.erows[self.cy].content.len();
            }
        } else if key == EKey::ARROW_RIGHT {
            if row.is_some() && self.cx < row.unwrap().content.len() {
                self.cx += 1;
            } else if row.is_some() {
                // implicitly also self.cx == row.unwrap().len()
                self.cy += 1;
                self.cx = 0;
            }
        } else if key == EKey::ARROW_DOWN {
            // this condition causes us to go one line below the last line in text file
            if self.cy < self.erows.len() {
                self.cy += 1;
            }
        } else if key == EKey::ARROW_UP {
            if self.cy != 0 {
                self.cy -= 1;
            }
        }

        // when going down / up, we might end up with a cursor that is past the length of that row
        let row_after_len = self.erows.get(self.cy).map_or(0, |s| s.content.len());
        if self.cx > row_after_len {
            self.cx = row_after_len;
        }
    }

    fn prompt(
        &mut self,
        format_str: &str,
        callback: Option<fn(&mut Editor, &str, u32)>,
    ) -> Option<String> {
        // TODO: Rust doesn't support dynamic string formatting out of the box
        // Crates like dynfmt solve it, but just use simple replace now
        let mut buf = String::new();
        loop {
            self.set_status_msg(&format_str.replace("{}", &buf));
            self.refresh_screen();
            let key: u32 = read_key();

            if key == EKey::DEL_KEY || key == CTRL_H || key == EKey::BACKSPACE {
                buf.pop();
            } else if key == EKey::ESCAPE {
                self.set_status_msg("");
                if let Some(cb) = callback {
                    cb(self, &buf, key);
                }
                return None;
            } else if key == ENTER {
                if !buf.is_empty() {
                    // successfully got the filename
                    self.set_status_msg("");
                    if let Some(cb) = callback {
                        cb(self, &buf, key);
                    }
                    return Some(buf);
                }
            } else if key < 128 && !(key as u8 as char).is_control() {
                // TODO: hacky, but keep until full unicode support
                // NOTE: key < 128 ensures that it can be represented as u8
                buf.push(key as u8 as char);
            }

            if let Some(cb) = callback {
                cb(self, &buf, key);
            }
        }
    }

    fn process_keypress(&mut self) {
        static mut quit_times: u8 = QUIT_TIMES;
        let key: u32 = read_key();

        match key {
            ENTER => {
                self.insert_newline();
            }
            CTRL_Q => {
                if self.dirty && unsafe { quit_times > 0 } {
                    unsafe {
                        self.set_status_msg(&format!("WARNING!!! File has unsaved changes. Press Ctrl-Q {} more times to quit.",quit_times));
                        quit_times -= 1;
                    }
                    return;
                }

                io::stdout().write(b"\x1b[2J").unwrap();
                io::stdout().write(b"\x1b[H").unwrap();
                io::stdout().flush().unwrap();
                self.cleanup();
                std::process::exit(0);
            }
            CTRL_F => {
                self.find();
            }
            CTRL_S => match self.save() {
                Ok(len) => {
                    self.set_status_msg(&format!("{} bytes written to disk", len));
                }
                Err(err) => {
                    self.set_status_msg(&format!("Couldn't save! {}", err.to_string()));
                }
            },
            EKey::PAGE_DOWN | EKey::PAGE_UP => {
                // note: on macOS, we need to press Shift + Fn + Up/Down arrows to register it as Page Up/Down
                let times = self.screen_rows;
                let arrow_key;
                if key == (EKey::PAGE_UP as u32) {
                    self.cy = self.roffset; // position cursor at the top of screen
                    arrow_key = EKey::ARROW_UP;
                } else {
                    self.cy = self.roffset + self.screen_rows - 1;
                    if self.cy > self.erows.len() {
                        self.cy = self.erows.len();
                    }
                    arrow_key = EKey::ARROW_DOWN;
                }

                for _ in 0..times {
                    self.move_cursor(arrow_key as u32);
                }
            }
            EKey::ARROW_UP | EKey::ARROW_DOWN | EKey::ARROW_LEFT | EKey::ARROW_RIGHT => {
                self.move_cursor(key);
            }
            EKey::HOME_KEY => {
                self.cx = 0;
            }
            EKey::END_KEY => {
                if self.cy < self.erows.len() {
                    self.cx = self.erows[self.cy].content.len();
                }
            }
            EKey::BACKSPACE | CTRL_H | EKey::DEL_KEY => {
                if key == EKey::DEL_KEY {
                    self.move_cursor(EKey::ARROW_RIGHT);
                }
                self.del_char();
            }
            EKey::ESCAPE | CTRL_L => {}
            _ => {
                // at this point we know that the only possible return value is in u8 because we exhaustively matched all enums
                // that fall outside the u8 range
                self.insert_char(key as u8 as char);
            }
        }

        unsafe {
            quit_times = QUIT_TIMES;
        }
    }

    /** file i/o */
    fn erows_to_str(&self) -> String {
        self.erows
            .iter()
            .map(|row| row.content.clone())
            .collect::<Vec<String>>()
            .join("\n")
    }

    fn save(&mut self) -> Result<usize, FileSaveError> {
        if self.filename.is_none() {
            self.filename = self.prompt("Save as: {} (Esc to cancel)", None);
            if self.filename.is_none() {
                self.set_status_msg("Save aborted");
                return Err(FileSaveError::NoFileSpecifiedError);
            }
        }

        let content = self.erows_to_str();
        let mut file = File::create(self.filename.as_ref().unwrap())?;
        file.set_len(content.len() as u64)?;
        file.write_all(content.as_bytes())?;
        self.dirty = false;
        Ok(content.len())
    }

    /** search */
    fn find_callback(&mut self, query: &str, key: u32) {
        // i64 makes conversion from usize safe
        static mut SEARCH_LAST_MATCH: i64 = -1;
        static mut SEARCH_DIR: i64 = 1;

        static mut SAVED_HL_LINE: Option<usize> = None;
        static mut SAVED_HL: Option<Vec<HLType>> = None;

        unsafe {
            if let Some(saved_hl) = &SAVED_HL {
                self.erows[SAVED_HL_LINE.unwrap()]
                    .hl
                    .copy_from_slice(saved_hl);
                SAVED_HL = None;
            }
        }

        if key == ENTER || key == EKey::ESCAPE {
            unsafe {
                SEARCH_LAST_MATCH = -1;
                SEARCH_DIR = 1;
            }
            return;
        } else if key == EKey::ARROW_DOWN || key == EKey::ARROW_RIGHT {
            // next match
            unsafe {
                SEARCH_DIR = 1;
            }
        } else if key == EKey::ARROW_UP || key == EKey::ARROW_LEFT {
            // prev match
            unsafe {
                SEARCH_DIR = -1;
            }
        } else {
            unsafe {
                SEARCH_LAST_MATCH = -1;
                SEARCH_DIR = 1;
            }
        }

        if unsafe { SEARCH_LAST_MATCH == -1 } {
            unsafe {
                SEARCH_DIR = 1;
            }
        }

        let mut current = unsafe { SEARCH_LAST_MATCH };
        for _ in 0..self.erows.len() {
            current += unsafe { SEARCH_DIR };
            if current == -1 {
                current = (self.erows.len() - 1) as i64;
            }
            if current == self.erows.len() as i64 {
                current = 0;
            }

            let nrows = self.erows.len();
            let row = &mut self.erows[current as usize];
            if let Some(byte_offset) = row.render.find(query) {
                unsafe {
                    SEARCH_LAST_MATCH = current;
                    SAVED_HL_LINE = Some(current as usize); // current guaranteed to be >= 0
                    SAVED_HL = Some(row.hl.to_vec());
                }

                self.cy = current as usize;
                self.cx = rx_to_cx(&row.content, byte_offset);
                self.roffset = nrows;

                // update matched highlight text color
                row.hl[byte_offset..byte_offset + query.len()].fill(HLType::Match);

                break;
            }
        }
    }

    fn find(&mut self) {
        let saved_cx = self.cx;
        let saved_cy = self.cy;
        let saved_coffset = self.coffset;
        let saved_roffset = self.roffset;

        if self
            .prompt(
                "Search: {} (Use ESC/Arrows/Enter)",
                Some(Editor::find_callback),
            )
            .is_none()
        {
            self.cx = saved_cx;
            self.cy = saved_cy;
            self.coffset = saved_coffset;
            self.roffset = saved_roffset;
        }
    }

    pub fn cleanup(&self) {
        disable_raw_mode(&self.orig_term);
    }
}
