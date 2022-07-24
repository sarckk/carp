extern crate libc;

mod constant;
mod errors;
mod helpers;

use constant::*;
use errors::FileSaveError;
use helpers::*;

use std::cmp;
use std::fs::File;
use std::io::{self, Write};
use std::time::Instant;
use termios::*;

/** highlights */
#[derive(Clone, Copy, PartialEq)]
pub enum HLType {
    Normal = 0,
    Number,
    Match,
    String,
    Comment,
    MLComment,
    Keyword1,
    Keyword2,
}

impl HLType {
    pub fn to_ansi_color(&self) -> u8 {
        match self {
            HLType::Number => 31,  // red
            HLType::Match => 34,   // blue
            HLType::String => 35,  // magenta
            HLType::Comment => 36, // cyan
            HLType::MLComment => 36,
            HLType::Keyword1 => 33, // yellow
            HLType::Keyword2 => 32, // green
            _ => 37,
        }
    }
}

struct Row {
    pub idx: usize,
    pub content: String,
    pub render: String,
    pub hl: Vec<HLType>, // 0 - 255 number codes
    pub hl_open_comment: bool,
}
#[derive(Clone)]
pub struct EditorSyntax {
    filetype: &'static str,
    filematch: &'static [&'static str],
    keywords: &'static [&'static str],
    single_line_comment_start: &'static str,
    multiline_comment_start: &'static str,
    multiline_comment_end: &'static str,
    flags: u8,
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
    syntax: Option<EditorSyntax>,
}

impl Editor {
    pub fn new() -> Self {
        let orig_term = enable_raw_mode();

        let (screen_rows, screen_cols) = get_win_size().unwrap_or_else(|err| {
            die("getWindowSize", &err.to_string());
        });

        Self {
            cx: 0,
            rx: 0,
            cy: 0,
            screen_rows: screen_rows - STATUS_HEIGHT,
            screen_cols,
            orig_term,
            erows: Vec::new(),
            roffset: 0,
            coffset: 0,
            filename: None,
            status_msg: None,
            status_time: Instant::now(),
            dirty: false,
            syntax: None,
        }
    }

    pub fn open(&mut self, filename_opt: Option<&String>) -> &mut Self {
        self.filename = filename_opt.map(|s| s.to_owned());
        self.update_syntax();

        let lines = read_file(filename_opt).unwrap_or_else(|err| {
            die("fopen", &err.to_string());
        });

        for line in lines {
            self.insert_row_at(self.erows.len(), line);
        }

        self.dirty = false;

        self
    }

    fn update_syntax(&mut self) {
        self.syntax = None;

        if self.filename.is_none() {
            return;
        }

        let name = self.filename.as_deref().unwrap();
        let ext = name.rfind('.');

        for hldb_entry in HLDB {
            for &pattern in hldb_entry.filematch {
                let is_ext = pattern.starts_with('.');
                // Rust compares values behind references (&str) due to default implementation of PartialEq<&str> for &str
                if (is_ext && ext.is_some() && &name[ext.unwrap()..] == pattern)
                    || (!is_ext && name.contains(pattern))
                {
                    self.syntax = Some(hldb_entry.clone());

                    for i in 0..self.erows.len() {
                        self.row_update_syntax(i);
                    }

                    return;
                }
            }
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
                        let c = render_row.as_bytes()[i];
                        if c.is_ascii_control() {
                            let sym = if c <= 26 { '@' as u8 + c } else { '?' as u8 };
                            buf.push_str("\x1b[7m"); // inverted bg color
                            buf.push(sym as char);
                            buf.push_str("\x1b[m"); // restore default
                        } else if hl_row[i] == HLType::Normal {
                            if current_color != -1 {
                                buf.push_str("\x1b[39m"); // default font color
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
    fn insert_row_at<T>(&mut self, at: usize, content: T)
    where
        T: Into<String>,
    {
        if at > self.erows.len() {
            return;
        }
        self.erows.insert(
            at,
            Row {
                idx: at,
                content: content.into(),
                render: String::new(),
                hl: Vec::new(),
                hl_open_comment: false,
            },
        );

        for i in at + 1..self.erows.len() {
            self.erows[i].idx += 1;
        }

        self.row_update(at);

        self.dirty = true;
    }

    fn del_row(&mut self, at: usize) {
        if at >= self.erows.len() {
            return;
        }
        self.erows.remove(at);

        for i in at..self.erows.len() {
            self.erows[i].idx -= 1;
        }

        self.dirty = true;
    }

    /** editor operations  */
    fn row_update_syntax(&mut self, row_idx: usize) {
        let (left, _right) = self.erows.split_at(row_idx);
        // NOTE: we need to set the value of in_comment here first so the borrow checker doesn't complain!
        let mut in_comment = left.last().map(|row| row.hl_open_comment).unwrap_or(false);

        let row = &mut self.erows[row_idx];
        let mut hl = vec![HLType::Normal; row.render.len()];

        if self.syntax.is_none() {
            row.hl = hl;
            return;
        }

        let mut i = 0;
        let mut prev_sep = true; // count beginning of line as preceding a separator

        let mut in_string = '\0'; // if not current in string, this should be NULL char

        let esyntax = self.syntax.as_ref().unwrap();

        let scs = esyntax.single_line_comment_start;
        let mcs = esyntax.multiline_comment_start;
        let mce = esyntax.multiline_comment_end;

        while i < row.render.len() {
            // TODO: Support unicode properly as .len() above on string returns no. of bytes not characters
            let c = row.render.as_bytes()[i] as char;
            let prev_hl = if i == 0 { HLType::Normal } else { hl[i - 1] };

            // highlight comments
            if !scs.is_empty() && in_string == '\0' && !in_comment {
                if row.render[i..].starts_with(scs) {
                    hl[i..].fill(HLType::Comment);
                    break;
                }
            }

            // multi-line comments
            if !mcs.is_empty() && !mce.is_empty() && in_string == '\0' {
                if in_comment {
                    hl[i] = HLType::MLComment;
                    if row.render[i..].starts_with(mce) {
                        hl[i..i + mce.len()].fill(HLType::MLComment);
                        i += mce.len();
                        in_comment = false;
                        prev_sep = true;
                        continue;
                    } else {
                        i += 1;
                        continue;
                    }
                } else if row.render[i..].starts_with(mcs) {
                    hl[i..i + mcs.len()].fill(HLType::MLComment);
                    i += mcs.len();
                    in_comment = true;
                    continue;
                }
            }

            if (esyntax.flags & HL_HIGHLIGHT_STRINGS) != 0 {
                if in_string != '\0' {
                    hl[i] = HLType::String;

                    if c == in_string {
                        in_string = '\0';
                    }

                    if c == '\\' && i + 1 < row.render.len() {
                        hl[i + 1] = HLType::String;
                        i += 2;
                        continue;
                    }

                    i += 1;
                    prev_sep = true;
                    continue;
                } else {
                    // string literals start with either " or '
                    if c == '"' || c == '\'' {
                        in_string = c;
                        hl[i] = HLType::String;
                        i += 1;
                        continue;
                    }
                }
            }

            if (esyntax.flags & HL_HIGHLIGHT_NUMBERS) != 0 {
                if (c.is_ascii_digit() && (prev_sep || prev_hl == HLType::Number))
                    || (c == '.' && prev_hl == HLType::Number)
                {
                    hl[i] = HLType::Number;
                }
            }

            if prev_sep {
                // TODO: clean up
                // keyword must be preceded and followed by a separator
                let mut found = false;

                for &keyw in esyntax.keywords {
                    let mut kw = keyw;
                    let kw2: bool = keyw.ends_with('|');
                    if kw2 {
                        kw = &kw[..kw.len() - 1];
                    }
                    let wlen = kw.len();

                    if row.render[i..].starts_with(kw)
                        && (i+wlen == row.render.len() // NOTE: need to do this because rust strings arent terminated with '\0'
                            || is_separator(row.render.as_bytes()[i+wlen] as char))
                    {
                        found = true;
                        let htype = if kw2 {
                            HLType::Keyword2
                        } else {
                            HLType::Keyword1
                        };
                        hl[i..(i + wlen)].fill(htype);
                        i += wlen;
                        break;
                    }
                }

                if found {
                    prev_sep = false;
                    continue;
                }
            }

            prev_sep = is_separator(c);
            i += 1;
        }
        row.hl = hl;

        let changed = row.hl_open_comment != in_comment;
        row.hl_open_comment = in_comment;
        if changed && row_idx + 1 < self.erows.len() {
            self.row_update_syntax(row_idx + 1);
        }
    }

    fn row_update(&mut self, row_idx: usize) {
        let row = &mut self.erows[row_idx];
        row.render.clear(); // ensure that render is empty before populating it again

        // replace all tab characters in the line with appropriate number of spaces
        let mut tabs = 0;
        for ch in row.content.chars() {
            if ch == '\t' {
                row.render.push(' ');
                if (tabs + 1) % TAB_WIDTH != 0 {
                    let nspaces = TAB_WIDTH - ((tabs + 1) % TAB_WIDTH);
                    let tab_spaces = " ".repeat(nspaces);
                    row.render.push_str(&tab_spaces);
                }
                tabs = 0;
            } else {
                row.render.push(ch);
                tabs += 1;
            }
        }
        self.row_update_syntax(row_idx);
    }

    fn row_insert_char(&mut self, row_idx: usize, at: usize, c: char) {
        let row = &mut self.erows[row_idx];
        row.content.insert(cmp::min(at, row.content.len()), c);
        self.row_update(row_idx);
    }

    fn row_del_char(&mut self, row_idx: usize, at: usize) {
        let row = &mut self.erows[row_idx];
        if at >= row.content.len() {
            return;
        }
        row.content.remove(at);
        self.row_update(row_idx);
    }

    fn row_append_str(&mut self, row_idx: usize, s: &str) {
        let row = &mut self.erows[row_idx];
        row.content.push_str(s);
        self.row_update(row_idx);
    }

    fn insert_char(&mut self, c: char) {
        if self.cy == self.erows.len() {
            // one line after the last text, so we start a new empty row
            self.insert_row_at(self.erows.len(), "");
        }

        self.row_insert_char(self.cy, self.cx, c);
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
            self.row_del_char(self.cy, self.cx - 1);
            self.cx -= 1;
        } else {
            self.cx = self.erows[self.cy - 1].content.len();
            let content = &self.erows[self.cy].content.clone();
            self.row_append_str(self.cy - 1, content);
            self.del_row(self.cy);
            self.cy -= 1;
        }
        self.dirty = true;
    }

    fn insert_newline(&mut self) {
        if self.cx == 0 {
            self.insert_row_at(self.cy, "");
        } else {
            // TODO: change all this when handling unicode correctly
            let next_line = self.erows[self.cy]
                .content
                .chars()
                .skip(self.cx)
                .collect::<String>();
            self.insert_row_at(self.cy + 1, next_line);

            let row = &mut self.erows[self.cy];
            row.content = row.content.chars().take(self.cx).collect();
            self.row_update(self.cy);
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
            "{:.20} - {} lines {}",
            filename,
            self.erows.len(),
            file_dirty_msg
        );
        let mut len = cmp::min(status_msg.len(), self.screen_cols);
        buf.push_str(&status_msg[..len]);

        let filetype = self
            .syntax
            .as_ref()
            .map(|syntax_struct| syntax_struct.filetype)
            .unwrap_or("no ft");

        let row_stat_msg = format!("{} | {}/{}", filetype, self.cy + 1, self.erows.len());

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
                // at this point we know that the only possible return value is in u8
                // because we exhaustively matched all enums that fall outside the u8 range
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
            self.update_syntax();
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
