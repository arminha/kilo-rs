extern crate libc;
extern crate termios;

use libc::{STDIN_FILENO, STDOUT_FILENO, TIOCGWINSZ, winsize};
use termios::{Termios, tcsetattr, TCSAFLUSH, VMIN, VTIME};
use termios::{BRKINT, ICRNL, INPCK, ISTRIP, IXON};
use termios::{OPOST, CS8};
use termios::{ECHO, ICANON, IEXTEN, ISIG};

use std::io::{self, Stdin, Stdout, Read, Error, ErrorKind, Write};

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

const ARROW_LEFT: u32 = 1000;
const ARROW_RIGHT: u32 = 1001;
const ARROW_UP: u32 = 1002;
const ARROW_DOWN: u32 = 1003;

macro_rules! ctrl_key {
    ($k:expr) => (($k & 0x1f) as u32);
}

struct RawMode {
    orig_term: Termios
}

struct Editor {
    _mode: RawMode,
    cx: u16,
    cy: u16,
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

fn read_non_blocking<R : Read>(r : &mut R, buf: &mut [u8]) -> usize {
    r.read(buf)
     .or_else(|e| if e.kind() == ErrorKind::WouldBlock { Ok(0) } else { Err(e) })
     .expect("read_non_blocking")
}

fn editor_read_key(stdin: &mut Stdin) -> u32 {
    let mut buf = [0; 1];
    loop {
        let n = read_non_blocking(stdin, &mut buf);
        if n == 1 {
            if buf[0] == b'\x1b' {
                let mut seq = [0; 2];
                let n = read_non_blocking(stdin, &mut seq);
                if n == 2 && seq[0] == b'[' {
                    return match seq[1] {
                        b'A' => ARROW_UP,
                        b'B' => ARROW_DOWN,
                        b'C' => ARROW_RIGHT,
                        b'D' => ARROW_LEFT,
                        _ => b'\x1b' as u32,
                    }
                } else {
                    return b'\x1b' as u32;
                }
            } else {
                return buf[0] as u32;
            }
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
        Ok(Editor {
            _mode: mode,
            cx: 0,
            cy: 0,
            screenrows: rows,
            screencols: cols,
            stdin: stdin,
            stdout: stdout
        })
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

        let move_cursor = format!("\x1b[{};{}H", self.cy + 1, self.cx + 1).into_bytes();
        self.write(&move_cursor)?;

        self.write(b"\x1b[?25h")?;
        self.flush()
    }

    fn move_cursor(&mut self, c: u32) {
        match c {
            ARROW_UP => {
                self.cy -= 1;
            },
            ARROW_DOWN => {
                self.cy += 1;
            },
            ARROW_LEFT => {
                self.cx -= 1;
            },
            ARROW_RIGHT => {
                self.cx += 1;
            },
            _ => {},
        }
    }

    fn process_keypress(&mut self) -> bool {
        let c = editor_read_key(&mut self.stdin);

        match c {
            k if k == ctrl_key!(b'q') => {
                false
            },
            ARROW_UP | ARROW_DOWN | ARROW_LEFT | ARROW_RIGHT => {
                self.move_cursor(c);
                true
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
