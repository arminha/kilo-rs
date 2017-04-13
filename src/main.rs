extern crate libc;
extern crate termios;

use libc::STDIN_FILENO;
use termios::{Termios, tcsetattr, TCSAFLUSH, VMIN, VTIME};
use termios::{BRKINT, ICRNL, INPCK, ISTRIP, IXON};
use termios::{OPOST, CS8};
use termios::{ECHO, ICANON, IEXTEN, ISIG};

use std::io::{self, Stdin, Stdout, Read, ErrorKind, Write};

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

fn editor_read_key(stdin: &mut Stdin) -> u8 {
    let mut buf = [0; 1];
    loop {
        let n = stdin.read(&mut buf)
                 .or_else(|e| if e.kind() == ErrorKind::WouldBlock { Ok(0) } else { Err(e) })
                 .expect("read");
        if n == 1 {
            return buf[0];
        }
    }
}

fn clear_screen(stdout: &mut Stdout) -> io::Result<()> {
    stdout.write(b"\x1b[2J")?;
    stdout.write(b"\x1b[H")?;
    stdout.flush()
}

fn editor_refresh_screen(stdout: &mut Stdout) -> io::Result<()> {
    clear_screen(stdout)
}

fn editor_process_keypress(stdin: &mut Stdin) -> bool {
    let c = editor_read_key(stdin);

    match c {
        k if k == ctrl_key!(b'q') => {
            false
        },
        _ => true
    }
}

fn main() {
    let _reset = enable_raw_mode().unwrap();

    let mut stdin = io::stdin();
    let mut stdout = io::stdout();
    editor_refresh_screen(&mut stdout).unwrap();
    while editor_process_keypress(&mut stdin) {
        editor_refresh_screen(&mut stdout).unwrap();
    }
    clear_screen(&mut stdout).unwrap();
}
