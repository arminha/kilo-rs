extern crate libc;
extern crate termios;

use libc::{STDIN_FILENO, STDOUT_FILENO, TIOCGWINSZ, winsize};
use termios::{Termios, tcsetattr, TCSAFLUSH, VMIN, VTIME};
use termios::{BRKINT, ICRNL, INPCK, ISTRIP, IXON};
use termios::{OPOST, CS8};
use termios::{ECHO, ICANON, IEXTEN, ISIG};

use std::env;
use std::fs::File;
use std::io::{self, Stdin, Stdout, Read, BufRead, BufReader, Error, ErrorKind, Write};

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

#[derive(PartialEq, Clone, Copy)]
enum Key {
    Character(u8),
    ArrowLeft,
    ArrowRight,
    ArrowUp,
    ArrowDown,
    DeleteKey,
    HomeKey,
    EndKey,
    PageUp,
    PageDown,
}

macro_rules! ctrl_key {
    ($k:expr) => ($k & 0x1f);
}

struct RawMode {
    orig_term: Termios
}

struct Row {
    chars: String,
}

struct Editor {
    _mode: RawMode,
    cx: u16,
    cy: u16,
    screenrows: u16,
    screencols: u16,
    numrows: u32,
    row: Option<Row>,
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

fn truncate_bytes(s: &str, max_len: u16) -> &[u8] {
    if s.len() > max_len as usize {
        s[..(max_len as usize)].as_bytes()
    } else {
        s.as_bytes()
    }
}

fn editor_read_key(stdin: &mut Stdin) -> Key {
    let mut buf = [0; 1];
    loop {
        let n = read_non_blocking(stdin, &mut buf);
        if n == 1 {
            if buf[0] == b'\x1b' {
                let mut seq = [0; 2];
                let n = read_non_blocking(stdin, &mut seq);
                if n == 2 && seq[0] == b'[' {
                    if seq[1] >= b'0' && seq[1] <= b'9' {
                        let mut last = [0; 1];
                        let n = read_non_blocking(stdin, &mut last);
                        if n == 1 {
                            if last[0] == b'~' {
                                return match seq[1] {
                                    b'1' => Key::HomeKey,
                                    b'3' => Key::DeleteKey,
                                    b'4' => Key::EndKey,
                                    b'5' => Key::PageUp,
                                    b'6' => Key::PageDown,
                                    b'7' => Key::HomeKey,
                                    b'8' => Key::EndKey,
                                    _ => Key::Character(b'\x1b'),
                                }
                            }
                        }
                    } else {
                        return match seq[1] {
                            b'A' => Key::ArrowUp,
                            b'B' => Key::ArrowDown,
                            b'C' => Key::ArrowRight,
                            b'D' => Key::ArrowLeft,
                            b'H' => Key::HomeKey,
                            b'F' => Key::EndKey,
                            _ => Key::Character(b'\x1b'),
                        }
                    }
                } else if n == 2 && seq[0] == b'O' {
                    return match seq[1] {
                        b'H' => Key::HomeKey,
                        b'F' => Key::EndKey,
                        _ => Key::Character(b'\x1b'),
                    }
                }
                return Key::Character(b'\x1b');
            } else {
                return Key::Character(buf[0]);
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
            numrows: 0,
            row: None,
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
            if y as u32 >= self.numrows {
                if self.numrows == 0 && y == self.screenrows / 3 {
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
            } else {
                let ref row = self.row.as_ref().unwrap().chars;
                self.stdout.write(truncate_bytes(row, self.screencols))?;
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

    fn move_cursor(&mut self, k: Key) {
        match k {
            Key::ArrowUp => {
                if self.cy > 0 {
                    self.cy -= 1;
                }
            },
            Key::ArrowDown => {
                if self.cy < self.screenrows - 1 {
                    self.cy += 1;
                }
            },
            Key::ArrowLeft => {
                if self.cx > 0 {
                    self.cx -= 1;
                }
            },
            Key::ArrowRight => {
                if self.cx < self.screencols - 1 {
                    self.cx += 1;
                }
            },
            _ => (),
        }
    }

    fn process_keypress(&mut self) -> bool {
        let c = editor_read_key(&mut self.stdin);

        match c {
            Key::Character(k) if k == ctrl_key!(b'q') => {
                return false
            },
            Key::HomeKey => {
                self.cx = 0;
            },
            Key::EndKey => {
                self.cx = self.screencols - 1;
            },
            Key::PageUp | Key::PageDown => {
                let key = if c == Key::PageUp { Key::ArrowUp } else { Key::ArrowDown };
                for _ in 0..(self.screenrows) {
                    self.move_cursor(key);
                }
            },
            Key::ArrowUp | Key::ArrowDown | Key::ArrowLeft | Key::ArrowRight => {
                self.move_cursor(c);
            },
            _ => ()
        };
        true
    }

    fn open(&mut self, filename: &str) -> io::Result<()> {
        let f = File::open(filename)?;
        let mut file = BufReader::new(&f);
        if let Some(line) = file.lines().next() {
            self.numrows = 1;
            self.row = Some(Row { chars: line? });
        }
        Ok(())
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
    if let Some(filename) = env::args().nth(1) {
        editor.open(&filename).unwrap();
    }

    editor.refresh_screen().unwrap();
    while editor.process_keypress() {
        editor.refresh_screen().unwrap();
    }
}
