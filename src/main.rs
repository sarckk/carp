use termios::*;
use std::io;
use std::io::Read;
use std::error::Error;

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

fn disable_raw_mode(mut orig_termios: Termios) {
    if let Err(err) = tcsetattr(0, TCSAFLUSH, &mut orig_termios) {
        die("tcsetattr", &err.to_string());
    }
}

fn is_control(c: u8) -> bool {
    return (c >= 0 && c <= 31) || c == 127;
}

// note: diverging function
fn die(msg: &str, error: &str) -> ! {
    eprintln!("{}: {}", msg, error);
    std::process::exit(1);
}

fn main() {
    let orig_termios = enable_raw_mode();

    let mut reader = io::stdin();

    loop {
        let mut buffer = [0;1];
        reader.read_exact(&mut buffer).ok();

        let c = buffer[0];

        if is_control(c) {
            print!("{:?}\r\n", c);
        } else  {
            print!("{:?} ({})\r\n", c, c as char);
        }

        if (buffer[0] as char) == 'q' {
            break;
        }
    }

    disable_raw_mode(orig_termios);
}
