extern crate libc;
extern crate termios;

use libc::{winsize, STDIN_FILENO, STDOUT_FILENO, TIOCGWINSZ};
use termios::{tcsetattr, Termios, TCSAFLUSH};

use std::borrow::Cow;
use std::cmp;
use std::env;
use std::fs::{File, OpenOptions};
use std::io::{self, BufRead, BufReader, Error, ErrorKind, Read, Stdin, Stdout, Write};
use std::time::{Duration, Instant};

const VERSION: &str = env!("CARGO_PKG_VERSION");

const TAB_STOP: usize = 8;

const KILO_QUIT_TIMES: u8 = 3;

macro_rules! ctrl_key {
    ($k:expr) => {
        $k & 0x1f
    };
}

const CTRL_Q: u8 = ctrl_key!(b'q');
const CTRL_H: u8 = ctrl_key!(b'h');
const CTRL_S: u8 = ctrl_key!(b's');
const CTRL_F: u8 = ctrl_key!(b'f');
const BACKSPACE: u8 = 127;

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
    rows: Vec<Row>,
    dirty: bool,
    quit_times: u8,
    stdin: Stdin,
    stdout: Stdout,
    filename: Option<String>,
    statusmsg: String,
    statusmsg_time: Instant,
}

impl RawMode {
    fn enable_raw_mode() -> io::Result<RawMode> {
        use termios::*;
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
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &self.orig_term).expect("Failed to drop RawMode")
    }
}

impl Row {
    fn new<T>(s: T) -> Row
    where
        T: Into<String>,
    {
        let chars = s.into();
        let render = Row::render_row(&chars);
        Row { chars, render }
    }

    fn render_row(chars: &str) -> String {
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
        render
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

    fn rx_to_cx(&self, rx: usize) -> usize {
        let mut cur_rx = 0;
        for (cx, ch) in self.chars.chars().enumerate() {
            if ch == '\t' {
                cur_rx += (TAB_STOP - 1) - (cur_rx % TAB_STOP);
            }
            cur_rx += 1;
            if cur_rx > rx {
                return cx;
            }
        }
        self.chars.len()
    }

    fn insert_char(&mut self, at: usize, c: char) {
        let idx = if at > self.chars.len() {
            self.chars.len()
        } else {
            at
        };
        self.chars.insert(idx, c);
        self.render = Row::render_row(&self.chars);
    }

    fn delete_char(&mut self, at: usize) {
        if at >= self.chars.len() {
            return;
        }
        self.chars.remove(at);
        self.render = Row::render_row(&self.chars);
    }

    fn append_str(&mut self, s: &str) {
        self.chars.push_str(s);
        self.render = Row::render_row(&self.chars);
    }

    fn truncate(&mut self, new_len: usize) -> String {
        let removed = self.chars[new_len..].to_string();
        self.chars.truncate(new_len);
        self.render = Row::render_row(&self.chars);
        removed
    }
}

fn read_non_blocking<R: Read>(r: &mut R, buf: &mut [u8]) -> usize {
    r.read(buf)
        .or_else(|e| {
            if e.kind() == ErrorKind::WouldBlock {
                Ok(0)
            } else {
                Err(e)
            }
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
            return Err(Error::new(
                ErrorKind::Other,
                "get_window_size: ioctl failed",
            ));
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
            screenrows: (rows - 2) as usize,
            screencols: cols as usize,
            rows: Vec::new(),
            dirty: false,
            quit_times: KILO_QUIT_TIMES,
            stdin,
            stdout,
            filename: None,
            statusmsg: String::new(),
            statusmsg_time: Instant::now(),
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
            self.rx = self.rows[self.cy].cx_to_rx(self.cx);
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
        self.rows.len()
    }

    fn draw_rows(&mut self) -> io::Result<()> {
        let numrows = self.numrows();
        for y in 0..(self.screenrows) {
            let filerow = y + self.rowoff;
            if filerow >= numrows {
                if self.filename.is_none() && self.rows.is_empty() && y == self.screenrows / 3 {
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
                self.stdout.write_all(byte_slice(
                    &self.rows[filerow].render,
                    self.coloff,
                    self.screencols,
                ))?;
            }

            self.write(b"\x1b[K")?;
            self.write(b"\r\n")?;
        }
        Ok(())
    }

    fn draw_status_bar(&mut self) -> io::Result<()> {
        self.write(b"\x1b[7m")?;
        let status;
        {
            let name = self.filename.as_ref().map_or("[No name]", |s| s.as_str());
            let modified = if self.dirty { " (modified)" } else { "" };
            let mut content = format!("{:.20} - {} lines{}", name, self.numrows(), modified);
            content.truncate(self.screencols);
            status = content;
        }
        let rstatus = format!("{}/{}", self.cy + 1, self.numrows());

        self.write(status.as_bytes())?;
        let mut len = status.len();
        while len < self.screencols {
            if self.screencols - len == rstatus.len() {
                self.write(rstatus.as_bytes())?;
                break;
            } else {
                self.write(b" ")?;
                len += 1;
            }
        }
        self.write(b"\x1b[m")?;
        self.write(b"\r\n")
    }

    fn draw_message_bar(&mut self) -> io::Result<()> {
        self.write(b"\x1b[K")?;
        if !self.statusmsg.is_empty() && self.statusmsg_time.elapsed() < Duration::from_secs(5) {
            let mut msg = Cow::from(self.statusmsg.as_str());
            if msg.len() > self.screencols {
                msg.to_mut().truncate(self.screencols);
            }
            self.stdout.write_all(msg.as_bytes())?;
        }
        Ok(())
    }

    fn try_refresh_screen(&mut self) -> io::Result<()> {
        self.scroll();

        self.write(b"\x1b[?25l")?;
        self.write(b"\x1b[H")?;

        self.draw_rows()?;
        self.draw_status_bar()?;
        self.draw_message_bar()?;

        let move_cursor = format!(
            "\x1b[{};{}H",
            (self.cy - self.rowoff) + 1,
            (self.rx - self.coloff) + 1
        )
        .into_bytes();
        self.write(&move_cursor)?;

        self.write(b"\x1b[?25h")?;
        self.flush()
    }

    fn refresh_screen(&mut self) {
        self.try_refresh_screen().expect("Failed to refresh screen");
    }

    fn set_status_message<S: Into<String>>(&mut self, msg: S) {
        self.statusmsg = msg.into();
        self.statusmsg_time = Instant::now();
    }

    fn rowlen(&self, index: usize) -> usize {
        let row = self.rows.get(index);
        row.map_or(0, |r| r.chars.len())
    }

    fn prompt<F, C>(&mut self, format_prompt: F, mut callback: C) -> Option<String>
    where
        F: Fn(&str) -> String,
        C: FnMut(&mut Editor, &str, Key) -> (),
    {
        let mut buf = String::new();
        loop {
            self.set_status_message(format_prompt(&buf));
            self.refresh_screen();

            let k = editor_read_key(&mut self.stdin);
            match k {
                Key::Delete | Key::Character(CTRL_H) | Key::Character(BACKSPACE) => {
                    buf.pop();
                }
                Key::Character(b'\x1b') => {
                    self.set_status_message("");
                    callback(self, &buf, k);
                    return None;
                }
                Key::Character(b'\r') => {
                    if !buf.is_empty() {
                        self.set_status_message("");
                        callback(self, &buf, k);
                        return Some(buf);
                    }
                }
                Key::ArrowLeft | Key::ArrowRight | Key::ArrowUp | Key::ArrowDown => {
                    callback(self, &buf, k);
                }
                Key::Character(c) if c >= 32 && c < 127 => {
                    buf.push(c as char);
                    callback(self, &buf, k);
                }
                _ => (),
            }
        }
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
                let row = self.rows.get(self.cy);
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

    fn insert_char(&mut self, c: char) {
        if self.cy == self.rows.len() {
            self.rows.push(Row::new(""));
        }
        self.rows[self.cy].insert_char(self.cx, c);
        self.cx += 1;
        self.dirty = true;
    }

    fn insert_new_line(&mut self) {
        if self.cx == 0 {
            self.rows.insert(self.cy, Row::new(""));
        } else {
            let new_line = self.rows[self.cy].truncate(self.cx);
            self.rows.insert(self.cy + 1, Row::new(new_line));
        }
        self.cy += 1;
        self.cx = 0;
    }

    fn delete_char(&mut self) {
        if self.cy == self.rows.len() {
            return;
        }
        if self.cy == 0 && self.cx == 0 {
            return;
        }
        if self.cx > 0 {
            self.cx -= 1;
            self.rows[self.cy].delete_char(self.cx);
            self.dirty = true;
        } else {
            let right = self.rows.remove(self.cy);
            self.cy -= 1;
            let left = &mut self.rows[self.cy];
            self.cx = left.chars.len();
            left.append_str(right.chars.as_str());
            self.dirty = true;
        }
    }

    fn process_keypress(&mut self) -> bool {
        let c = editor_read_key(&mut self.stdin);

        match c {
            Key::Character(b'\r') => self.insert_new_line(),
            Key::Character(CTRL_Q) => {
                if self.dirty && self.quit_times > 0 {
                    let msg = format!(
                        "WARNING!!! File has unsaved changes. \
                         Press Ctrl-Q {} more times to quit.",
                        self.quit_times
                    );
                    self.set_status_message(msg);
                    self.quit_times -= 1;
                    return true;
                }
                return false;
            }
            Key::Character(CTRL_S) => self.save(),
            Key::Home => {
                self.cx = 0;
            }
            Key::End => {
                self.cx = self.rowlen(self.cy);
            }
            Key::Character(CTRL_F) => self.find(),
            Key::Character(CTRL_H) | Key::Character(BACKSPACE) => self.delete_char(),
            Key::Delete => {
                self.move_cursor(Key::ArrowRight);
                self.delete_char();
            }
            Key::PageUp | Key::PageDown => {
                let key = if c == Key::PageUp {
                    self.cy = self.rowoff;
                    Key::ArrowUp
                } else {
                    self.cy = cmp::min(self.rowoff + self.screenrows - 1, self.numrows());
                    Key::ArrowDown
                };
                for _ in 0..(self.screenrows) {
                    self.move_cursor(key);
                }
            }
            Key::ArrowUp | Key::ArrowDown | Key::ArrowLeft | Key::ArrowRight => {
                self.move_cursor(c);
            }
            Key::Character(k) if k >= 32 && k < 127 => self.insert_char(k as char),
            _ => (),
        };
        self.quit_times = KILO_QUIT_TIMES;
        true
    }

    fn open(&mut self, filename: &str) -> io::Result<()> {
        self.filename = Some(filename.to_owned());
        let f = File::open(filename)?;
        let file = BufReader::new(&f);
        let results: io::Result<Vec<Row>> = file.lines().map(|r| r.map(Row::new)).collect();
        self.rows = results?;
        self.dirty = false;
        Ok(())
    }

    fn save_to_file(&mut self) -> io::Result<usize> {
        let filename = match self.filename {
            Some(ref f) => f,
            None => return Ok(0),
        };
        let mut data: Vec<u8> = Vec::new();
        for row in &self.rows {
            writeln!(data, "{}", &row.chars)?;
        }
        let mut file = OpenOptions::new().write(true).create(true).open(filename)?;
        file.set_len(data.len() as u64)?;
        file.write_all(&data)?;
        self.dirty = false;
        Ok(data.len())
    }

    fn save(&mut self) {
        if self.filename.is_none() {
            self.filename = self.prompt(|v| format!("Save as: {}", v), |_, _, _| ());
            if self.filename.is_none() {
                self.set_status_message("Save aborted");
                return;
            }
        }
        match self.save_to_file() {
            Ok(size) => self.set_status_message(format!("{} bytes written to disk", size)),
            Err(e) => self.set_status_message(format!("Can't save! I/O error: {}", e)),
        }
    }

    fn find(&mut self) {
        let saved_cx = self.cx;
        let saved_cy = self.cy;
        let saved_coloff = self.coloff;
        let saved_rowoff = self.rowoff;
        let mut last_match: Option<usize> = None;

        let callback = |editor: &mut Editor, query: &str, key: Key| {
            let forward = match key {
                Key::Character(b'\x1b') | Key::Character(b'\r') => {
                    last_match = None;
                    return;
                }
                Key::ArrowRight | Key::ArrowDown => true,
                Key::ArrowLeft | Key::ArrowUp => last_match.is_none(),
                _ => {
                    last_match = None;
                    true
                }
            };

            // TODO find a way to remove this code duplication
            if forward {
                let first_row = last_match.map_or(0, |l| l + 1);
                let rows = editor.rows.iter().enumerate();
                let rotate_rows = rows.clone().skip(first_row).chain(rows.take(first_row));
                for (i, row) in rotate_rows {
                    if let Some(idx) = row.render.find(&query) {
                        last_match = Some(i);
                        editor.cy = i;
                        editor.cx = row.rx_to_cx(idx);
                        editor.rowoff = editor.rows.len();
                        break;
                    }
                }
            } else {
                let first_row = editor.rows.len() - last_match.unwrap();
                let reverse_rows = editor.rows.iter().enumerate().rev();
                let rotate_rows = reverse_rows
                    .clone()
                    .skip(first_row)
                    .chain(reverse_rows.take(first_row));
                for (i, row) in rotate_rows {
                    if let Some(idx) = row.render.find(&query) {
                        last_match = Some(i);
                        editor.cy = i;
                        editor.cx = row.rx_to_cx(idx);
                        editor.rowoff = editor.rows.len();
                        break;
                    }
                }
            }
        };

        let query = self.prompt(
            |v| format!("Search: {} (Use ESC/Arrows/Enter)", v),
            callback,
        );
        if query.is_none() {
            self.cx = saved_cx;
            self.cy = saved_cy;
            self.coloff = saved_coloff;
            self.rowoff = saved_rowoff;
        }
    }
}

impl Drop for Editor {
    fn drop(&mut self) {
        fn clear_screen(editor: &mut Editor) -> io::Result<()> {
            editor.stdout.write_all(b"\x1b[2J")?;
            editor.stdout.write_all(b"\x1b[H")?;
            editor.stdout.flush()
        }

        clear_screen(self).expect("Failed to clear screen");
    }
}

fn main() -> io::Result<()> {
    let mut editor = Editor::new()?;
    if let Some(filename) = env::args().nth(1) {
        editor.open(&filename)?;
    }

    editor.set_status_message("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

    editor.refresh_screen();
    while editor.process_keypress() {
        editor.refresh_screen();
    }
    Ok(())
}
