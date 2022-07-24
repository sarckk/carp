mod editor;

use editor::Editor;
use std::env;
use std::panic::catch_unwind;
use termios::{tcsetattr, Termios, TCSAFLUSH};

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = args.get(1);

    // NOTE: see multi-threaded version@
    // https://stackoverflow.com/questions/43441047/whats-the-best-way-to-register-a-function-to-run-during-an-unexpected-exit-of-a
    let orig_term = Termios::from_fd(0).unwrap();
    match catch_unwind(|| Editor::new().open(filename).run()) {
        Ok(_) => std::process::exit(0),
        Err(_) => {
            tcsetattr(0, TCSAFLUSH, &orig_term).unwrap();
            std::process::exit(1);
        }
    }
}
