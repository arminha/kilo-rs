extern crate libc;
extern crate termios;

use libc::STDIN_FILENO;
use termios::{Termios, tcsetattr, TCSAFLUSH, VMIN, VTIME};
use termios::{BRKINT, ICRNL, INPCK, ISTRIP, IXON};
use termios::{OPOST, CS8};
use termios::{ECHO, ICANON, IEXTEN, ISIG};

use std::io::{self, Read, ErrorKind};

macro_rules! ctrl_key {
    ($k:expr) => ($k & 0x1f);
}

struct TermReset {
    orig_term: Termios
}

impl Drop for TermReset {
    fn drop(&mut self) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &self.orig_term).unwrap()
    }
}

fn enable_raw_mode() -> io::Result<TermReset> {
    let mut term = Termios::from_fd(STDIN_FILENO)?;
    let mode = TermReset { orig_term: term.clone() };

    term.c_iflag &= !(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    term.c_oflag &= !OPOST;
    term.c_cflag |= CS8;
    term.c_lflag &= !(ECHO | ICANON | IEXTEN | ISIG);
    term.c_cc[VMIN] = 0;
    term.c_cc[VTIME] = 1;

    tcsetattr(STDIN_FILENO, TCSAFLUSH, &term)?;
    Ok(mode)
}

fn iscntrl(c: u8) -> bool {
    unsafe {
        libc::iscntrl(c as i32) != 0
    }
}

fn main() {
    let _reset = enable_raw_mode().unwrap();

    let mut stdin = io::stdin();
    let mut buf = [0; 1];

    loop {
        buf[0] = 0;
        let n = stdin.read(&mut buf)
                     .or_else(|e| if e.kind() == ErrorKind::WouldBlock { Ok(0) } else { Err(e) })
                     .expect("read");
        if n > 0 {
            let c = buf[0];
            if c == ctrl_key!(b'q') {
                break;
            }
            if iscntrl(c) {
                print!("{}\r\n", &c);
            } else {
                print!("{} ({})\r\n", &c, c as char);
            }
        }
    }
}
