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

const TAB_STOP: usize = 8;

#[derive(PartialEq, Clone, Copy)]
enum Key {
    Character(u8),
    ArrowLeft,
    ArrowRight,
    ArrowUp,
    ArrowDown,
    Delete,
    Home,
    End,
    PageUp,
    PageDown,
}

macro_rules! ctrl_key {
    ($k:expr) => ($k & 0x1f);
}

struct RawMode {
    orig_term: Termios,
}

struct Row {
    chars: String,
    render: String,
}

struct Editor {
    _mode: RawMode,
    cx: usize,
    cy: usize,
    rx: usize,
    rowoff: usize,
    coloff: usize,
    screenrows: usize,
    screencols: usize,
    rows: Option<Vec<Row>>,
    stdin: Stdin,
    stdout: Stdout,
}

impl RawMode {
    fn enable_raw_mode() -> io::Result<RawMode> {
        let mut term = Termios::from_fd(STDIN_FILENO)?;
        let mode = RawMode { orig_term: term };

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

impl Row {
    fn new<T>(s: T) -> Row
        where T: Into<String>
    {
        let chars = s.into();
        let mut idx = 0;
        let mut render = String::new();
        for ch in chars.chars() {
            if ch == '\t' {
                render.push(' ');
                idx += 1;
                while idx % TAB_STOP != 0 {
                    render.push(' ');
                    idx += 1;
                }
            } else {
                render.push(ch);
                idx += 1;
            }
        }
        Row { chars, render }
    }

    fn cx_to_rx(&self, cx: usize) -> usize {
        let mut rx = 0;
        for ch in self.chars.chars().take(cx) {
            if ch == '\t' {
                rx += (TAB_STOP - 1) - (rx % TAB_STOP);
            }
            rx += 1;
        }
        rx
    }
}

fn read_non_blocking<R: Read>(r: &mut R, buf: &mut [u8]) -> usize {
    r.read(buf)
        .or_else(|e| if e.kind() == ErrorKind::WouldBlock {
                     Ok(0)
                 } else {
                     Err(e)
                 })
        .expect("read_non_blocking")
}

fn byte_slice(s: &str, offset: usize, max_len: usize) -> &[u8] {
    if s.len() > max_len + offset {
        s[offset..(max_len + offset)].as_bytes()
    } else if s.len() > offset {
        s[offset..].as_bytes()
    } else {
        s[0..0].as_bytes()
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
                        if n == 1 && last[0] == b'~' {
                            return match seq[1] {
                                       b'1' | b'7' => Key::Home,
                                       b'3' => Key::Delete,
                                       b'4' | b'8' => Key::End,
                                       b'5' => Key::PageUp,
                                       b'6' => Key::PageDown,
                                       _ => Key::Character(b'\x1b'),
                                   };
                        }
                    } else {
                        return match seq[1] {
                                   b'A' => Key::ArrowUp,
                                   b'B' => Key::ArrowDown,
                                   b'C' => Key::ArrowRight,
                                   b'D' => Key::ArrowLeft,
                                   b'H' => Key::Home,
                                   b'F' => Key::End,
                                   _ => Key::Character(b'\x1b'),
                               };
                    }
                } else if n == 2 && seq[0] == b'O' {
                    return match seq[1] {
                               b'H' => Key::Home,
                               b'F' => Key::End,
                               _ => Key::Character(b'\x1b'),
                           };
                }
                return Key::Character(b'\x1b');
            } else {
                return Key::Character(buf[0]);
            }
        }
    }
}

fn get_window_size() -> io::Result<(u16, u16)> {
    let ws = winsize {
        ws_col: 0,
        ws_row: 0,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };
    unsafe {
        if libc::ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0 {
            return Err(Error::new(ErrorKind::Other, "get_window_size: ioctl failed"));
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
               rx: 0,
               rowoff: 0,
               coloff: 0,
               screenrows: rows as usize,
               screencols: cols as usize,
               rows: None,
               stdin: stdin,
               stdout: stdout,
           })
    }

    fn write(&mut self, buf: &[u8]) -> io::Result<()> {
        self.stdout.write_all(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.stdout.flush()
    }

    fn scroll(&mut self) {
        self.rx = self.cx;
        if self.cy < self.numrows() {
            self.rx = self.rows.as_ref().unwrap()[self.cy].cx_to_rx(self.cx);
        }

        if self.cy < self.rowoff {
            self.rowoff = self.cy;
        }
        if self.cy >= self.rowoff + self.screenrows {
            self.rowoff = self.cy - self.screenrows + 1;
        }
        if self.rx < self.coloff {
            self.coloff = self.rx;
        }
        if self.rx >= self.coloff + self.screencols {
            self.coloff = (1 + self.rx) - self.screencols;
        }
    }

    fn numrows(&self) -> usize {
        self.rows.as_ref().map_or(0, |v| v.len())
    }

    fn draw_rows(&mut self) -> io::Result<()> {
        let numrows = self.numrows();
        for y in 0..(self.screenrows) {
            let filerow = y + self.rowoff;
            if filerow >= numrows {
                if self.rows.is_none() && y == self.screenrows / 3 {
                    let mut msg = format!("Kilo-rs editor -- version {}", VERSION);
                    msg.truncate(self.screencols);
                    let padding = (self.screencols - msg.len()) / 2;
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
                let rows = self.rows.as_ref().unwrap();
                self.stdout
                    .write_all(byte_slice(&rows[filerow].render, self.coloff, self.screencols))?;
            }

            self.write(b"\x1b[K")?;
            if y < self.screenrows - 1 {
                self.write(b"\r\n")?;
            }
        }
        Ok(())
    }

    fn refresh_screen(&mut self) -> io::Result<()> {
        self.scroll();

        self.write(b"\x1b[?25l")?;
        self.write(b"\x1b[H")?;

        self.draw_rows()?;

        let move_cursor = format!("\x1b[{};{}H",
                                  (self.cy - self.rowoff) + 1,
                                  (self.rx - self.coloff) + 1)
                .into_bytes();
        self.write(&move_cursor)?;

        self.write(b"\x1b[?25h")?;
        self.flush()
    }

    fn rowlen(&self, index: usize) -> usize {
        let row = self.rows.as_ref().and_then(|r| r.get(index));
        row.map_or(0, |r| r.chars.len())
    }

    fn move_cursor(&mut self, k: Key) {
        match k {
            Key::ArrowUp => {
                if self.cy > 0 {
                    self.cy -= 1;
                }
            }
            Key::ArrowDown => {
                if self.cy < self.numrows() {
                    self.cy += 1;
                }
            }
            Key::ArrowLeft => {
                if self.cx > 0 {
                    self.cx -= 1;
                } else if self.cy > 0 {
                    self.cy -= 1;
                    self.cx = self.rowlen(self.cy);
                }
            }
            Key::ArrowRight => {
                let row = self.rows.as_ref().and_then(|r| r.get(self.cy));
                let rowlen = row.map_or(0, |r| r.chars.len());
                if self.cx < rowlen {
                    self.cx += 1;
                } else if row.is_some() && self.cx == rowlen {
                    self.cx = 0;
                    self.cy += 1;
                }
            }
            _ => (),
        }

        let rowlen = self.rowlen(self.cy);
        if self.cx > rowlen {
            self.cx = rowlen;
        }
    }

    fn process_keypress(&mut self) -> bool {
        let c = editor_read_key(&mut self.stdin);

        match c {
            Key::Character(k) if k == ctrl_key!(b'q') => return false,
            Key::Home => {
                self.cx = 0;
            }
            Key::End => {
                self.cx = self.screencols - 1;
            }
            Key::PageUp | Key::PageDown => {
                let key = if c == Key::PageUp {
                    Key::ArrowUp
                } else {
                    Key::ArrowDown
                };
                for _ in 0..(self.screenrows) {
                    self.move_cursor(key);
                }
            }
            Key::ArrowUp | Key::ArrowDown | Key::ArrowLeft | Key::ArrowRight => {
                self.move_cursor(c);
            }
            _ => (),
        };
        true
    }

    fn open(&mut self, filename: &str) -> io::Result<()> {
        let f = File::open(filename)?;
        let file = BufReader::new(&f);
        let results: io::Result<Vec<Row>> = file.lines().map(|r| r.map(Row::new)).collect();
        self.rows = Some(results?);
        Ok(())
    }
}

impl Drop for Editor {
    fn drop(&mut self) {
        // clear screen
        self.stdout.write_all(b"\x1b[2J").unwrap();
        self.stdout.write_all(b"\x1b[H").unwrap();
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
