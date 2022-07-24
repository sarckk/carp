use super::EditorSyntax;

#[non_exhaustive]
pub struct EKey;

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

const fn ctrl(c: char) -> u32 {
    (c as u32) & 0x1f
}

pub const CARP_VER: &str = "0.0.1";

pub const TAB_WIDTH: usize = 8;
pub const STATUS_HEIGHT: usize = 2;
pub const QUIT_TIMES: u8 = 3;

pub const EAGAIN: i32 = 11;

pub const ENTER: u32 = '\r' as u32;
pub const CTRL_Q: u32 = ctrl('q');
pub const CTRL_F: u32 = ctrl('f');
pub const CTRL_H: u32 = ctrl('h');
pub const CTRL_L: u32 = ctrl('l');
pub const CTRL_S: u32 = ctrl('s');

pub const HL_HIGHLIGHT_NUMBERS: u8 = 1 << 0;
pub const HL_HIGHLIGHT_STRINGS: u8 = 1 << 1;

pub const C_HL_EXT: &[&'static str] = &[".c", ".h", ".cpp"];
pub const C_HL_KEYWORDS: &[&'static str] = &[
    "switch",
    "if",
    "while",
    "for",
    "break",
    "continue",
    "return",
    "else",
    "struct",
    "union",
    "typedef",
    "static",
    "enum",
    "class",
    "case",
    "int|",
    "long|",
    "double|",
    "float|",
    "char|",
    "unsigned|",
    "signed|",
    "void|",
];

pub const HLDB: &[EditorSyntax] = &[EditorSyntax {
    filetype: "c",
    filematch: C_HL_EXT,
    keywords: C_HL_KEYWORDS,
    single_line_comment_start: "//",
    multiline_comment_start: "/*",
    multiline_comment_end: "*/",
    flags: HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
}];
