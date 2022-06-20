mod editor;

use termios::{Termios, TCSAFLUSH, tcsetattr};
use editor::Editor;
use std::panic::catch_unwind;

fn main() {
    // NOTE: see multi-threaded version@ 
    // https://stackoverflow.com/questions/43441047/whats-the-best-way-to-register-a-function-to-run-during-an-unexpected-exit-of-a 
    let mut orig_term = Termios::from_fd(0).unwrap();
    match catch_unwind(|| Editor::new().run()) {
        Ok(_) => std::process::exit(0),
        Err(_) => {
            tcsetattr(0, TCSAFLUSH, &orig_term);
            std::process::exit(1);
        }
    }
}
