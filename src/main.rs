extern crate libc;
extern crate termios;

use libc::{STDIN_FILENO, STDOUT_FILENO, TIOCGWINSZ, winsize};
use termios::{Termios, tcsetattr, TCSAFLUSH, VMIN, VTIME};
use termios::{BRKINT, ICRNL, INPCK, ISTRIP, IXON};
use termios::{OPOST, CS8};
use termios::{ECHO, ICANON, IEXTEN, ISIG};

use std::io::{self, Stdin, Stdout, Read, Error, ErrorKind, Write};

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

macro_rules! ctrl_key {
    ($k:expr) => ($k & 0x1f);
}

struct TermReset {
    orig_term: Termios
}

struct EditorConfig {
    _term: TermReset,
    screenrows: u16,
    screencols: u16,
}

struct Editor {
    config: EditorConfig,
    stdin: Stdin,
    stdout: Stdout,
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

impl EditorConfig {
    fn new() -> EditorConfig {
        let term = enable_raw_mode().unwrap();
        let (rows, cols) = get_window_size().unwrap();
        EditorConfig { _term: term, screenrows: rows, screencols: cols }
    }
}

impl Editor {
    fn new(config: EditorConfig) -> Editor {
        let stdin = io::stdin();
        let stdout = io::stdout();
        Editor { config: config, stdin: stdin, stdout: stdout }
    }
}

impl Drop for Editor {
    fn drop(&mut self) {
        self.stdout.write(b"\x1b[2J").unwrap();
        self.stdout.write(b"\x1b[H").unwrap();
        self.stdout.flush().unwrap();
    }
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

fn get_window_size() -> io::Result<(u16, u16)> {
    let ws = winsize { ws_col: 0, ws_row: 0, ws_xpixel: 0, ws_ypixel: 0 };
    unsafe {
        if libc::ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0 {
            return Err(Error::new(ErrorKind::Other, "get_window_size: ioctl failed"))
        }
    }
    Ok((ws.ws_row, ws.ws_col))
}

fn editor_draw_rows(stdout: &mut Stdout, e: &EditorConfig) -> io::Result<()> {
    for y in 0..(e.screenrows) {
        if y == e.screenrows / 3 {
            let mut msg = format!("Kilo-rs editor -- version {}", VERSION);
            msg.truncate(e.screencols as usize);
            let padding = (e.screencols - msg.len() as u16) / 2;
            if padding > 0 {
                stdout.write(b"~")?;
                for _ in 1..padding {
                    stdout.write(b" ")?;
                }
            }
            stdout.write(msg.as_bytes())?;
        } else {
            stdout.write(b"~")?;
        }

        stdout.write(b"\x1b[K")?;
        if y < e.screenrows - 1 {
            stdout.write(b"\r\n")?;
        }
    }
    Ok(())
}

fn editor_refresh_screen(stdout: &mut Stdout, e: &EditorConfig) -> io::Result<()> {
    stdout.write(b"\x1b[?25l")?;
    stdout.write(b"\x1b[H")?;

    editor_draw_rows(stdout, e)?;

    stdout.write(b"\x1b[H")?;
    stdout.write(b"\x1b[?25h")?;
    stdout.flush()
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
    let mut editor = Editor::new(EditorConfig::new());

    editor_refresh_screen(&mut editor.stdout, &editor.config).unwrap();
    while editor_process_keypress(&mut editor.stdin) {
        editor_refresh_screen(&mut editor.stdout, &editor.config).unwrap();
    }
}
