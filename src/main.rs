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

struct RawMode {
    orig_term: Termios
}

struct Editor {
    _mode: RawMode,
    screenrows: u16,
    screencols: u16,
    stdin: Stdin,
    stdout: Stdout,
}

impl RawMode {
    fn enable_raw_mode() -> io::Result<RawMode> {
        let mut term = Termios::from_fd(STDIN_FILENO)?;
        let mode = RawMode { orig_term: term.clone() };

        term.c_iflag &= !(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
        term.c_oflag &= !OPOST;
        term.c_cflag |= CS8;
        term.c_lflag &= !(ECHO | ICANON | IEXTEN | ISIG);
        term.c_cc[VMIN] = 0;
        term.c_cc[VTIME] = 1;

        tcsetattr(STDIN_FILENO, TCSAFLUSH, &term)?;
        Ok(mode)
    }
}

impl Drop for RawMode {
    fn drop(&mut self) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &self.orig_term).unwrap()
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

impl Editor {
    fn new() -> io::Result<Editor> {
        let mode = RawMode::enable_raw_mode()?;
        let (rows, cols) = get_window_size()?;
        let stdin = io::stdin();
        let stdout = io::stdout();
        Ok(Editor { _mode: mode, screenrows: rows, screencols: cols, stdin: stdin, stdout: stdout })
    }

    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.stdout.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.stdout.flush()
    }

    fn draw_rows(&mut self) -> io::Result<()> {
        for y in 0..(self.screenrows) {
            if y == self.screenrows / 3 {
                let mut msg = format!("Kilo-rs editor -- version {}", VERSION);
                msg.truncate(self.screencols as usize);
                let padding = (self.screencols - msg.len() as u16) / 2;
                if padding > 0 {
                    self.write(b"~")?;
                    for _ in 1..padding {
                        self.write(b" ")?;
                    }
                }
                self.write(msg.as_bytes())?;
            } else {
                self.write(b"~")?;
            }

            self.write(b"\x1b[K")?;
            if y < self.screenrows - 1 {
                self.write(b"\r\n")?;
            }
        }
        Ok(())
    }

    fn refresh_screen(&mut self) -> io::Result<()> {
        self.write(b"\x1b[?25l")?;
        self.write(b"\x1b[H")?;

        self.draw_rows()?;

        self.write(b"\x1b[H")?;
        self.write(b"\x1b[?25h")?;
        self.flush()
    }

    fn process_keypress(&mut self) -> bool {
        let c = editor_read_key(&mut self.stdin);

        match c {
            k if k == ctrl_key!(b'q') => {
                false
            },
            _ => true
        }
    }
}

impl Drop for Editor {
    fn drop(&mut self) {
        // clear screen
        self.stdout.write(b"\x1b[2J").unwrap();
        self.stdout.write(b"\x1b[H").unwrap();
        self.stdout.flush().unwrap();
    }
}

fn main() {
    let mut editor = Editor::new().unwrap();

    editor.refresh_screen().unwrap();
    while editor.process_keypress() {
        editor.refresh_screen().unwrap();
    }
}
