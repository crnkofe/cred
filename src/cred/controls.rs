/**
 * MIT License
 *
 * Copyright (c) 2020 Simon Mihevc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
use regex::Regex;
use std::cmp;
use std::cmp::Ordering;
use std::env;
use std::error::Error;
use std::fs;
use std::fs::OpenOptions;
use std::include_str;
use std::io::prelude::*;
use std::io::BufReader;
use std::panic;
use std::path::Path;
use std::path::PathBuf;
use unicode_segmentation::UnicodeSegmentation;
use uuid::Uuid;

use std::time::{Duration, Instant};

use std::collections::HashMap;
use std::collections::LinkedList;

use super::common::{
    Buffer, ControlType, Coverage, FontStyle, Location, Size, UndoRedoAction, INVERSE_STYLE,
    NORMAL_STYLE,
};
use super::events::{
    CreateFileEvent, DialogType, Event, ExitEvent, GotoLineEvent, HandleEvent, HandleGotoEvent,
    HandleKey, HandleSearchEvent, HandleSelectEvent, HandleUndoEvent, HandleWindowEvent,
    OpenFileEvent, SearchDirection, SearchEvent, SelectAction, SelectEvent, UndoEvent,
    WindowAction, WindowEvent,
};
use super::syntax::*;

use rustbox::ExtendedKey;
use rustbox::Key;
use rustbox::{InitOptions, RustBox};

use clipboard::ClipboardContext;
use clipboard::ClipboardProvider;

use terminal_size::{terminal_size, Height, Width};

// TODO: move some of these shortcuts to configuration
const SPACE_STRING: &str = " ";
const SPACE: char = ' ';
const NEWLINE: char = '\n';
const TAB: char = '\t';

// for file buffer show line number % NUMBER_MOD
const NUMBER_MOD: usize = 100000;
// numbers larger than this value will be %mod-ed
const NUMBER_COUNT: usize = 5;

// depth to which dirs are loaded
const LOAD_DIRECTORY_DEPTH: usize = 5;

const OPEN_FILE_PRIORITY: usize = 600;
const MENU_PRIORITY: usize = 500;
const VISIBLE_FILE_BUFFER_PRIORITY: usize = 200;
const DEFAULT_FILE_BUFFER_PRIORITY: usize = 100;

// undo/redo max entries per file buffer
const MAX_ACTION_SIZE: usize = 10000;
// TODO: check for unizero on file load to see if binary file loaded
// const UNIZERO: char = '\u{0}';

// show help on any window
const HELP_SHORTCUT: Key = Key::F(1);

// ctrl+x shortcuts
const CTRL_MENU_SHORTCUT: char = 'd';
const CTRL_COPY_SHORTCUT: char = 'c';
const CTRL_PASTE_SHORTCUT: char = 'v';
// const CTRL_GOTO_SHORTCUT: char = 'g';
// TODO: detect keyboard layout and swap
const CTRL_UNDO_REDO_SHORTCUT: char = 'z';

// search shortcuts
const CTRL_SEARCH_SHORTCUT: char = 'f';
const SEARCH_FORWARD_SHORTCUT: char = 'f';
const SEARCH_BACKWARD_SHORTCUT: char = 'b';

// wsad movement shortcuts where applicable
const GAME_UP_SHORTCUT: char = 'w';
const GAME_DOWN_SHORTCUT: char = 's';
const GAME_LEFT_SHORTCUT: char = 'a';
const GAME_RIGHT_SHORTCUT: char = 'd';

const TAB_CHARS_COUNT: usize = 4;

const WINDOW_BORDER: [usize; 2] = [1, 1];

fn handle_key<T: HandleKey>(
    map: &mut HashMap<Uuid, T>,
    uuid: Uuid,
    ekey: ExtendedKey,
    buffer: Buffer,
) -> Option<Event> {
    match map.get_mut(&uuid) {
        Some(control) => Some(control.handle_key(ekey, buffer)),
        None => {
            log::error!("Unknown control: {}", uuid);
            None
        }
    }
}

fn get_renderable<T: Render>(map: &HashMap<Uuid, T>, uuid: Uuid) -> Option<&dyn Render> {
    match map.get(&uuid) {
        Some(control) => Some(control),
        None => {
            log::error!("Unknown control: {}", uuid);
            None
        }
    }
}

fn ord(c: char) -> i32 {
    (c as u8) as i32
}

// disable linter here since algorithm matches original pseudocode and is actually more
// understandable that way
#[allow(clippy::many_single_char_names)]
pub fn rabin_karp_search(pattern: String, text: &[char], prime: i32) -> Vec<usize> {
    // locations of found matching patterns
    let mut result: Vec<usize> = Vec::new();

    // assume 256 chars for alphabet for the moment - the reality is that we're dealing with unicode
    const D: i32 = 256;
    let m: i32 = pattern.len() as i32;
    let n: i32 = text.len() as i32;
    let mut j: i32 = 0;

    let mut p = 0; // hash value for pattern
    let mut t = 0; // hash value for txt
    let mut h = 1;

    // The value of h would be "pow(d, m-1)%q"
    for _ in 0..m - 1 {
        h = (h * D) % prime;
    }

    // Calculate the hash value of pattern and first
    // window of text
    for i in 0..m {
        let p_char = ord(pattern.chars().nth(i as usize).unwrap());
        p = (D * p + p_char) % prime;
        t = (D * t + ord(text[i as usize])) % prime;
    }

    // Slide the pattern over text one by one
    for i in 0..n - m {
        // Check the hash values of current window of text
        // and pattern. If the hash values match then only
        // check for characters on by one
        if p == t {
            /* Check for characters one by one */
            'inner: for k in 0..m {
                if text[(i + k) as usize] != pattern.chars().nth(k as usize).unwrap() {
                    break 'inner;
                }
                j = k;
            }

            // if p == t and pattern[0...m-1] = text[i, i+1, ...i+m-1]
            if j == (m - 1) {
                result.push(i as usize)
            }
            j = 0;
        }

        // Calculate hash value for next window of text: Remove
        // leading digit, add trailing digit
        if i < n - m {
            t = (D * (t - ord(text[i as usize]) * h) + ord(text[(i + m) as usize])) % prime;

            // We might get negative value of t, converting it
            // to positive
            if t < 0 {
                t += prime;
            }
        }
    }
    result
}

trait Window {
    fn get_origin(&self) -> Location {
        Location::new(0, 0)
    }

    fn get_size(&self) -> Size {
        Size::new(0, 0)
    }

    fn get_title(&self) -> String {
        String::from("")
    }

    fn render_window(&self, buffer: &Buffer) {
        // TODO: add window properties to struct
        // TODO: figure out width, height based on amount of menu items
        // TODO: render bottom or top and with different styles
        // write title with the following offset
        let title_offset = 2;
        let title_row_offset = buffer.editor_top_left.row;

        let top_left_edge = '\u{250C}';
        let top_right_edge = '\u{2510}';
        let bottom_left_edge = '\u{2514}';
        let bottom_right_edge = '\u{2518}';
        let horizontal_line = '\u{2500}';
        let vertical_line = '\u{2502}';

        let top_left = self.get_origin();
        let size = self.get_size();
        let title = self.get_title();
        // render outline
        for row in top_left.row..top_left.row + size.rows {
            for column in top_left.column..top_left.column + size.columns {
                let location = Location::new(row, column);
                let column_index = column - top_left.column;
                if row == top_left.row + title_row_offset
                    && column_index >= title_offset
                    && column_index < (title_offset + title.len())
                {
                    let mut char_string = String::from("");
                    match title.chars().nth(column_index - title_offset) {
                        Some(character) => {
                            char_string.push(character);
                        }
                        _ => {
                            continue;
                        }
                    }
                    buffer.write_direct(&char_string, location, NORMAL_STYLE);
                } else if row == top_left.row && column == top_left.column {
                    buffer.write_direct(&String::from(top_left_edge), location, NORMAL_STYLE);
                } else if row == top_left.row + size.rows - 1 && column == top_left.column {
                    buffer.write_direct(&String::from(bottom_left_edge), location, NORMAL_STYLE);
                } else if row == top_left.row && column == top_left.column + size.columns - 1 {
                    buffer.write_direct(&String::from(top_right_edge), location, NORMAL_STYLE);
                } else if row == top_left.row + size.rows - 1
                    && column == top_left.column + size.columns - 1
                {
                    buffer.write_direct(&String::from(bottom_right_edge), location, NORMAL_STYLE);
                } else if row == top_left.row || row == top_left.row + size.rows - 1 {
                    buffer.write_direct(&String::from(horizontal_line), location, NORMAL_STYLE);
                } else if column == top_left.column || column == top_left.column + size.columns - 1
                {
                    buffer.write_direct(&String::from(vertical_line), location, NORMAL_STYLE);
                } else {
                    buffer.write_direct(&String::from(SPACE_STRING), location, NORMAL_STYLE);
                }
            }
        }
    }
}

/**
 * Type of do/undoable action
 */
#[derive(Copy, Clone, Debug, PartialEq)]
enum ActionType {
    // insert string at
    Insert,
    // remove string from
    Remove,
    // paste is essentially a different kind of insert
    Paste,
}

#[derive(Clone, Debug)]
struct Action {
    // type of action
    action_type: ActionType,
    // text position at start of action
    // for removal start is at end of text
    // for insertion at first char
    start_location: usize,

    // for insertion
    content: String,
    // length of content or length of removal
    len: usize,
}

impl Action {
    pub fn insert(at: usize, content: String) -> Self {
        Self {
            action_type: ActionType::Insert,
            start_location: at,
            content: content.clone(),
            len: content.len(),
        }
    }

    pub fn remove(at: usize, content: String, length: usize) -> Self {
        Self {
            action_type: ActionType::Remove,
            start_location: at,
            content,
            len: length,
        }
    }

    pub fn paste(at: usize, content: String) -> Self {
        Self {
            action_type: ActionType::Paste,
            start_location: at,
            content: content.clone(),
            len: content.len(),
        }
    }
}

/**
 * Easy to use wrapper around Vec that allows conversion from-to line index to text location in
 * file
 */
#[derive(Clone, Debug)]
pub struct LineIndex {
    line_size: Vec<usize>,
}

// TODO: unit tests
impl LineIndex {
    pub fn new() -> Self {
        Self {
            line_size: Vec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.line_size.len()
    }

    pub fn location(&self, line: usize) -> Option<usize> {
        if line == 0 {
            return Some(0);
        }
        // if requested line beyond last location return None
        if line >= self.line_size.len() {
            return None;
        }
        let end_line = cmp::min(line, self.line_size.len() - 1);
        Some(self.line_size[0..end_line].iter().sum())
    }

    pub fn line_length(&self, line: usize) -> Option<usize> {
        // at the moment we don't know where the last line ends
        if line > (self.line_size.len() - 1) {
            return None;
        }
        Some(self.line_size[line])
    }

    /**
     * Returns (line index, line start)
     */
    pub fn line(&self, location: usize) -> Option<(usize, usize)> {
        if self.line_size.is_empty() {
            // noninitialized index
            return None;
        }

        let mut last_line_start = 0;
        let mut active_location: usize = 0;
        for (current_line_index, line_length) in self.line_size.iter().enumerate() {
            last_line_start = active_location;
            if location >= active_location && location < active_location + line_length {
                return Some((current_line_index, active_location));
            }
            active_location += line_length;
        }

        Some((self.line_size.len() - 1, last_line_start))
    }

    /**
     * Intended to be used only for initialization
     */
    pub fn push_line(&mut self, size: usize) {
        self.line_size.push(size);
    }

    pub fn break_line(&mut self, location: usize) {
        match self.line(location) {
            Some((line_index, line_start)) => {
                let old_size = self.line_size[line_index];
                // if breaking past last line push a new line
                if location >= line_start + old_size {
                    self.line_size.push(1);
                } else {
                    self.line_size[line_index] = location - line_start + 1;
                    self.line_size
                        .insert(line_index + 1, line_start + old_size - location);
                }
            }
            None => {
                self.line_size.push(1);
            }
        }
    }

    pub fn insert_character_at(&mut self, location: usize) {
        match self.line(location) {
            Some((line_index, _length)) => {
                self.line_size[line_index] += 1;
            }
            None => {
                log::warn!("Failed finding line at location: {:?}", location);
            }
        }
    }

    pub fn remove_line_at(&mut self, location: usize) {
        match self.line(location) {
            Some((line_index, _length)) => {
                let next_line_size = if line_index < self.line_size.len() - 1 {
                    self.line_size[line_index + 1]
                } else {
                    1
                };
                self.line_size[line_index] += next_line_size - 1; // minus newline
                if self.line_size.len() > line_index + 1 {
                    self.line_size.remove(line_index + 1);
                }
            }
            None => {
                log::warn!("Failed finding line at location: {:?}", location);
            }
        }
    }

    pub fn remove_character_at(&mut self, location: usize) {
        match self.line(location) {
            Some((line_index, _length)) => {
                self.line_size[line_index] -= 1;
            }
            None => {
                log::warn!("Failed finding line at location: {:?}", location);
            }
        }
    }
}

/**
 * All UI controls should implement render to allow for visual representation
 */
trait Render {
    fn render(&self, _buffer: &Buffer) {
        // noop
    }
}

// assembly of control references so we can group them into a single queue
#[derive(Copy, Clone, Debug)]
struct ControlReference {
    uuid: Uuid,
    priority: usize,
    control_type: ControlType,
}

impl ControlReference {
    pub fn new(control_type: ControlType, priority: usize) -> Self {
        Self {
            uuid: Uuid::new_v4(),
            priority,
            control_type,
        }
    }

    fn is_overlay(&self) -> bool {
        self.control_type == ControlType::SelectionOverlay
            || self.control_type == ControlType::UndoRedoOverlay
            || self.control_type == ControlType::SearchOverlay
            || self.control_type == ControlType::HelpOverlay
    }
}

impl Ord for ControlReference {
    fn cmp(&self, other: &Self) -> Ordering {
        self.priority.cmp(&other.priority)
    }
}

impl PartialOrd for ControlReference {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for ControlReference {
    fn eq(&self, other: &Self) -> bool {
        self.uuid == other.uuid
    }
}

impl Eq for ControlReference {}

// the reason for this static is that rustbox doesn't implement copy, clone
// so instead of copying it I just implemented it as a static var
// TODO: it would be nice to make this work without statics
static mut EDITOR_BUFFER: Option<RustBox> = None;

impl Buffer {
    pub fn new(rows: usize, columns: usize) -> Self {
        Self {
            size: Size::new(rows, columns),
            editor_top_left: Location::new(0, 0),
            editor_size: Size::new(rows, columns),
        }
    }

    fn hide_cursor(&self) {
        unsafe {
            match &EDITOR_BUFFER {
                Some(rust_box) => {
                    rust_box.hide_cursor();
                }
                None => {}
            }
        }
    }

    fn clear(&self) {
        unsafe {
            match &EDITOR_BUFFER {
                Some(rust_box) => {
                    rust_box.clear();
                }
                None => {}
            }
        }
    }

    fn clear_row(&self, current_location: Location, size: Size) {
        for remaining_column in current_location.column..self.size.columns {
            let remaining_location = Location::new(current_location.row, remaining_column);
            self.write(
                &String::from(SPACE_STRING),
                remaining_location,
                NORMAL_STYLE,
                Location::new(0, 0), // TODO
                size,
            );
        }
    }

    fn peek_event(&self, duration: Duration) -> Option<rustbox::EventResult> {
        unsafe {
            EDITOR_BUFFER
                .as_ref()
                .map(|rust_box| rust_box.peek_event(duration, false))
        }
    }

    fn present(&self) {
        unsafe {
            match &EDITOR_BUFFER {
                Some(rust_box) => {
                    rust_box.present();
                }
                None => {}
            }
        }
    }

    fn write(
        &self,
        string: &str,
        text_location: Location,
        style: FontStyle,
        top_left: Location,
        size: Size,
    ) {
        if !is_render_location_in_buffer(text_location, top_left, size) {
            return;
        }
        let cropped_location = text_location;
        unsafe {
            if let Some(rust_box) = &EDITOR_BUFFER {
                rust_box.print(
                    cropped_location.column,
                    cropped_location.row,
                    style.font_style,
                    style.foreground_color,
                    style.background_color,
                    string,
                );
            }
        }
    }

    fn write_direct(&self, string: &str, view_location: Location, style: FontStyle) {
        unsafe {
            if let Some(rust_box) = &EDITOR_BUFFER {
                rust_box.print(
                    view_location.column,
                    view_location.row,
                    style.font_style,
                    style.foreground_color,
                    style.background_color,
                    string,
                );
            }
        }
    }
}

/**
 * In-memory file data and state
 */
#[derive(Clone, Debug)]
pub struct FileBuffer {
    buffer_id: Uuid,
    // * flag section *
    // currently this only hides cursor
    // TODO: prevent editing in file buffer
    read_only: bool,
    show_line_number: bool,
    // * end flag section *
    //
    // syntax highlighting
    syntax: Syntax,

    // buffer contents
    contents: Vec<char>,
    // list of lines for quick access
    lines: LineIndex,

    // is selection mode on? if None no selection mode is enabled
    coverage: Option<Coverage>,
    // start of selection (on space)
    coverage_start: Option<usize>,
    // end of selection (on enter)
    coverage_end: Option<usize>,

    // current offset from start of line
    // is set in some cases and used when user presses up/down to maintain
    // aproximately the same editing position
    editing_offset: usize,

    // text pointer
    text_location: usize,
    // location of buffer window
    view_location: Location,
    // absolute path to file
    file_path: String,

    // do-undo related fields
    // active action is used for redo's so user can undo and redo
    // if user undo's a few times and then does something new redo history is lost
    active_action: usize,
    action_history: Vec<Action>,

    // search related fields
    pattern: String,
    current_match: usize,
    matches: Vec<usize>,
}

impl FileBuffer {
    fn new(buffer_id: Uuid, syntax: Syntax) -> Self {
        Self {
            buffer_id,
            syntax,
            read_only: false,
            show_line_number: false,
            file_path: "".to_owned(),
            lines: LineIndex::new(),
            coverage: None,
            coverage_start: None,
            coverage_end: None,
            editing_offset: 0,
            contents: Vec::new(),
            text_location: 0,
            view_location: Location::new(0, 0),
            active_action: 0,
            action_history: Vec::new(),
            pattern: String::from(""),
            current_match: 0,
            matches: Vec::new(),
        }
    }

    pub fn get_slice_string(&self, from: usize, to: usize) -> String {
        let mut data: Vec<u8> = vec![0; cmp::min(self.contents.len() - from, to - from)];
        for (i, c) in (self.contents[from..cmp::min(self.contents.len(), to)])
            .iter()
            .enumerate()
        {
            data[i] = *c as u8;
        }
        let converted_string = String::from_utf8(data);
        converted_string.unwrap()
    }

    fn write_to_buffer(&mut self, character: char) {
        // TODO: handle add new line and push lines right
        if character == NEWLINE {
            self.lines.break_line(self.text_location);
        } else {
            self.lines.insert_character_at(self.text_location);
        }

        self.contents.insert(self.text_location, character);
        self.text_location += 1;
    }

    fn remove_from_buffer(&mut self) {
        if self.text_location > self.contents.len() - 1 {
            return;
        }
        if self.contents[self.text_location] == NEWLINE {
            self.lines.remove_line_at(self.text_location);
        } else {
            self.lines.remove_character_at(self.text_location);
        }
        self.contents.remove(self.text_location);
    }

    #[allow(dead_code)]
    pub fn get_text_location(&self) -> usize {
        self.text_location
    }

    fn up(&mut self) {
        let now = Instant::now();

        let mut new_location = self.text_location;
        if new_location == 0 {
            return;
        }
        let start_of_line = self.find_start_of_line(new_location);
        if start_of_line == 0 {
            // on first line ignore up movement
            return;
        }
        let start_of_previous_line = self.find_start_of_line(start_of_line - 1);
        new_location = start_of_previous_line;
        self.text_location = new_location;

        self.move_on_line_right(self.editing_offset);
        log::info!(
            "[Performance] Move up: {:?}",
            Instant::now().duration_since(now)
        );
    }

    fn down(&mut self) {
        let now = Instant::now();
        let previous_text_location = self.text_location;
        self.text_location = self.find_next_line();
        if previous_text_location != self.text_location {
            self.move_on_line_right(self.editing_offset);
        }
        log::info!(
            "[Performance] Move up: {:?}",
            Instant::now().duration_since(now)
        );
    }

    fn find_start_of_line(&self, text_location: usize) -> usize {
        if text_location == 0 {
            return text_location;
        }

        let mut start_of_line = cmp::max(0, cmp::min(self.contents.len() - 1, text_location));
        if start_of_line > 0 && self.contents[start_of_line] == NEWLINE {
            start_of_line -= 1;
        }
        while start_of_line > 0 && self.contents[start_of_line] != NEWLINE {
            start_of_line -= 1;
        }

        if self.contents[start_of_line] == NEWLINE {
            start_of_line + 1
        } else {
            start_of_line
        }
    }

    fn find_end_of_line(&self) -> usize {
        let mut end_of_line = self.text_location;
        while end_of_line < self.contents.len() && self.contents[end_of_line] != NEWLINE {
            end_of_line += 1;
        }

        if end_of_line == self.contents.len() {
            return end_of_line - 1;
        }

        if self.contents[end_of_line] != NEWLINE {
            // double check
            return self.text_location;
        }

        end_of_line
    }

    fn find_next_line(&self) -> usize {
        let mut end_of_line = self.text_location;

        if self.contents.is_empty() {
            return 0;
        }

        if end_of_line >= self.contents.len() - 1 {
            return self.contents.len() - 1;
        }

        while end_of_line <= (self.contents.len() - 1) && self.contents[end_of_line] != NEWLINE {
            end_of_line += 1;
        }

        if end_of_line >= self.contents.len() - 1 {
            return self.text_location;
        }

        if self.contents[end_of_line] != NEWLINE {
            return self.text_location;
        }

        cmp::min(self.contents.len() - 1, end_of_line + 1)
    }

    fn move_on_line_right(&mut self, steps: usize) {
        let mut remaining_steps = steps;
        let mut text_location = self.text_location;
        while remaining_steps > 0
            && text_location <= (self.contents.len() - 1)
            && self.contents[text_location] != NEWLINE
        {
            if self.contents[text_location] == TAB {
                text_location += 1;
                remaining_steps -= cmp::min(remaining_steps, TAB_CHARS_COUNT);
            } else {
                text_location += 1;
                remaining_steps -= 1;
            }
        }
        self.text_location = text_location;
    }

    fn align_buffer_horizontal(&mut self, window_buffer: &Buffer) {
        let text_location = self.index_to_location();
        if !is_location_in_buffer(
            text_location,
            self.view_location,
            window_buffer.editor_top_left,
            window_buffer.editor_size,
        ) {
            self.view_location.column = window_buffer.editor_size.columns
                * (text_location.column / window_buffer.editor_size.columns);
        }
    }

    // TODO: I think all of these aligns could be merged
    fn align_buffer_vertical_up(&mut self, window_buffer: &Buffer) {
        let text_location = self.index_to_location();
        let mut next_row = self.view_location.row;
        while !is_location_in_buffer(
            text_location,
            Location::new(next_row, self.view_location.column),
            window_buffer.editor_top_left,
            window_buffer.editor_size,
        ) {
            if next_row == 0 {
                break;
            }

            if next_row > 0
                && is_location_in_buffer(
                    text_location,
                    Location::new(next_row - 1, self.view_location.column),
                    window_buffer.editor_top_left,
                    window_buffer.editor_size,
                )
            {
                next_row -= 1;
                break;
            }

            if next_row > window_buffer.editor_size.rows {
                next_row -= window_buffer.editor_size.rows;
            } else {
                next_row = 0;
            }
        }

        // TODO: manage start of file
        self.view_location = Location::new(next_row, self.view_location.column);
    }

    fn align_buffer_vertical_down(&mut self, window_buffer: &Buffer, allow_minimum: bool) {
        // check if pointer is out of buffer and scroll to find it
        let text_location = self.index_to_location();
        while !is_location_in_buffer(
            text_location,
            self.view_location,
            window_buffer.editor_top_left,
            window_buffer.editor_size,
        ) {
            if self.view_location.row + window_buffer.editor_size.rows > self.lines.len() {
                break;
            }

            let next_view_location =
                Location::new(self.view_location.row + 1, self.view_location.column);
            if allow_minimum
                && is_location_in_buffer(
                    text_location,
                    next_view_location,
                    window_buffer.editor_top_left,
                    window_buffer.editor_size,
                )
            {
                self.view_location = next_view_location;
            } else {
                self.view_location = Location::new(
                    self.view_location.row + window_buffer.editor_size.rows,
                    self.view_location.column,
                );
            }
        }
    }

    fn text_pointer_to_location(&self, text_location: usize, expand_tabs: bool) -> Location {
        match self.lines.line(text_location) {
            Some((line_index, line_start_location)) => {
                let row = line_index;
                let mut column = 0;
                for i in line_start_location..text_location {
                    if self.contents[i] == TAB && expand_tabs {
                        column += TAB_CHARS_COUNT;
                    } else {
                        column += 1;
                    }
                }
                Location::new(row, column)
            }
            None => {
                panic!("Development error - can't find text pointer.");
            }
        }
    }

    fn index_to_location(&self) -> Location {
        match self.lines.line(self.text_location) {
            Some((line_index, line_start_location)) => {
                let row = line_index;
                let mut column = 0;
                for i in line_start_location..self.text_location {
                    if self.contents[i] == TAB {
                        column += TAB_CHARS_COUNT;
                    } else {
                        column += 1;
                    }
                }
                Location::new(row, column)
            }
            None => {
                panic!("Development error - can't find text pointer.");
            }
        }
    }

    fn find_start_of_render(&self, view_location: Location) -> Option<usize> {
        self.lines.location(view_location.row).map(|location| {
            location
                + cmp::min(
                    self.lines
                        .line_length(view_location.row)
                        .unwrap_or(usize::MAX)
                        .saturating_sub(1),
                    view_location.column,
                )
        })
    }

    fn redo(&mut self, window_buffer: Buffer) {
        if self.active_action < self.action_history.len() {
            self.action_redo(self.active_action, window_buffer);
        }
    }

    fn undo(&mut self, window_buffer: Buffer) {
        if self.active_action > 0 {
            self.action_undo(self.active_action - 1, window_buffer);
            self.active_action -= 1;
        }
    }

    pub fn copy_clipboard(&mut self) {
        let ctx_option = ClipboardProvider::new();
        if ctx_option.is_err() {
            log::warn!("Failed fetching clipboard provider.");
            return;
        }

        let mut ctx: ClipboardContext = ctx_option.unwrap();
        // be ignorant and just copy everything
        match ctx.get_contents() {
            Ok(clipboard_contents) => {
                self.action_do(Action::paste(self.text_location, clipboard_contents));
            }
            Err(e) => {
                // TODO: show something nice to user
                log::info!("Failed copying from clipboard: {:?}", e);
            }
        }
    }

    pub fn to_clipboard(&self, contents: Vec<char>) {
        let ctx_option = ClipboardProvider::new();
        if ctx_option.is_err() {
            log::warn!("Failed fetching clipboard provider.");
            return;
        }

        let mut ctx: ClipboardContext = ctx_option.unwrap();
        match String::from_utf8(contents.iter().map(|x| *x as u8).collect()) {
            Ok(converted_string) => {
                if let Err(e) = ctx.set_contents(converted_string) {
                    log::warn!("Failed pasting to clipboard: {:?}", e);
                }
            }
            Err(e) => {
                log::info!("Failed converting input to UTF-8: {:?}", e);
            }
        }
    }

    fn handle_key_normal(&mut self, ekey: ExtendedKey, window_buffer: Buffer) -> Event {
        if ekey.modifiers.ctrl {
            if let Key::Char(input_control) = ekey.key {
                match input_control {
                    CTRL_MENU_SHORTCUT => {
                        return Event {
                            window_event: Some(WindowEvent::open(ControlType::Menu)),
                            ..Event::key(ekey)
                        }
                    }
                    CTRL_COPY_SHORTCUT => {
                        return Event {
                            window_event: Some(WindowEvent::open(ControlType::SelectionOverlay)),
                            ..Event::key(ekey)
                        }
                    }
                    CTRL_PASTE_SHORTCUT => {
                        self.copy_clipboard();
                    }
                    CTRL_UNDO_REDO_SHORTCUT => {
                        return Event {
                            window_event: Some(WindowEvent::open(ControlType::UndoRedoOverlay)),
                            ..Event::key(ekey)
                        }
                    }
                    CTRL_SEARCH_SHORTCUT => {
                        return Event {
                            window_event: Some(WindowEvent::open(ControlType::SearchOverlay)),
                            ..Event::key(ekey)
                        }
                    }
                    _ => {}
                }
            }
        } else {
            /*
             * TODO: Handle remaining chars
             * F(u32),
             * Unknown(u16),
             */
            match ekey.key {
                Key::Up => {
                    self.up();
                    self.align_buffer_vertical_up(&window_buffer);
                }
                Key::Down => {
                    self.down();
                    self.align_buffer_horizontal(&window_buffer);
                    self.align_buffer_vertical_down(&window_buffer, true);
                }
                Key::Right => {
                    self.text_location = cmp::min(self.contents.len() - 1, self.text_location + 1);
                    self.update_editing_offset(self.text_location);
                    self.align_buffer_horizontal(&window_buffer);
                }
                Key::Left => {
                    if self.text_location == 0 {
                        return Event::new();
                    }
                    self.text_location = cmp::max(0, self.text_location - 1);
                    self.update_editing_offset(self.text_location);
                    self.align_buffer_horizontal(&window_buffer);
                }
                Key::Backspace => {
                    if self.text_location > 0 {
                        let content = self.contents[self.text_location - 1].to_string();
                        self.action_do(Action::remove(self.text_location - 1, content, 1));
                        self.align_buffer_vertical_up(&window_buffer);
                        self.align_buffer_horizontal(&window_buffer);
                    }
                }
                Key::Delete => {
                    if self.text_location < (self.contents.len() - 1) {
                        let content = self.contents[self.text_location].to_string();
                        self.action_do(Action::remove(self.text_location, content, 1));
                    }
                }
                Key::Insert => {
                    // TODO: implement insert semantics
                    return Event::new();
                }
                Key::Home => {
                    self.text_location = self.find_start_of_line(self.text_location);

                    // scroll buffer to left
                    self.view_location.column = 0;

                    self.update_editing_offset(self.text_location);
                }
                Key::End => {
                    self.text_location = self.find_end_of_line();
                    self.align_buffer_horizontal(&window_buffer);
                    self.update_editing_offset(self.text_location);
                }
                Key::PageUp => {
                    // move down count lines
                    for _ in 0..window_buffer.editor_size.rows {
                        self.up();
                    }
                    self.align_buffer_vertical_up(&window_buffer);
                }
                Key::PageDown => {
                    if self.text_location >= self.contents.len() - 1 {
                        return Event::new();
                    }
                    for _ in 0..window_buffer.editor_size.rows {
                        self.down();
                    }
                    self.align_buffer_horizontal(&window_buffer);
                    self.align_buffer_vertical_down(&window_buffer, false);
                }
                Key::Esc => {
                    return Event {
                        window_event: Some(WindowEvent::open(ControlType::Menu)),
                        ..Event::new()
                    };
                }
                Key::Tab => {
                    self.action_do(Action::insert(self.text_location, TAB.to_string()));
                }
                Key::Enter => {
                    // smart tab
                    // TODO: respect either tab as tab characters or tab as spaces
                    let mut whitespace_count = 0;
                    let mut start = if self.text_location > 0 {
                        self.text_location - 1
                    } else {
                        0
                    };
                    while start > 0 && self.contents[start] != NEWLINE {
                        if self.contents[start] == TAB {
                            whitespace_count += TAB_CHARS_COUNT;
                        } else if self.contents[start].is_whitespace() {
                            whitespace_count += 1;
                        } else {
                            whitespace_count = 0;
                        }
                        start -= 1;
                    }

                    self.action_do(Action::insert(self.text_location, NEWLINE.to_string()));
                    // make auto tab insertion configurable
                    for _ in 0..whitespace_count {
                        self.action_do(Action::insert(self.text_location, SPACE.to_string()));
                    }
                    self.align_buffer_horizontal(&window_buffer);
                    self.align_buffer_vertical_down(&window_buffer, false);
                }
                HELP_SHORTCUT => {
                    return Event {
                        window_event: Some(WindowEvent::open(ControlType::HelpOverlay)),
                        ..Event::new()
                    };
                }
                Key::Char(input_char) => {
                    self.action_do(Action::insert(self.text_location, input_char.to_string()));
                }
                _ => {}
            }

            // TODO: should be g key - but ctrl+g isn't detected
            if Key::Unknown(7) == ekey.key {
                return Event {
                    window_event: Some(WindowEvent::open_dialog(
                        ControlType::InputDialog,
                        DialogType::Goto,
                    )),
                    ..Event::new()
                };
            }
        }

        Event::new()
    }

    fn update_editing_offset(&mut self, text_location: usize) {
        self.editing_offset = self.text_pointer_to_location(text_location, true).column;
    }

    fn set_select_mode(&mut self, coverage: Option<Coverage>) {
        match coverage.unwrap_or(Coverage::FromTo) {
            Coverage::FromTo => {
                self.coverage_start = Some(self.text_location);
                self.coverage_end = None;
            }
            Coverage::Word => {
                let mut start_word = self.text_location;
                let mut end_word = self.text_location;
                while start_word > 1 && self.contents[start_word - 1].is_ascii_alphanumeric() {
                    start_word -= 1;
                }
                while end_word < (self.contents.len() - 1)
                    && self.contents[end_word].is_ascii_alphanumeric()
                {
                    end_word += 1;
                }
                self.coverage_start = Some(start_word);
                self.coverage_end = Some(end_word);
            }
            Coverage::Line => {
                let location = self.index_to_location();
                let start = self.text_location - location.column;
                self.coverage_start = Some(start);

                let line_length = self
                    .lines
                    .line_length(location.row)
                    .unwrap_or(self.contents.len() - 1 - self.text_location);
                self.coverage_end = Some(start + line_length);
            }
        }
    }

    fn handle_key_selection(&mut self, ekey: ExtendedKey, window_buffer: Buffer) -> Event {
        /*
         * TODO: Handle remaining chars
         * Char(char),
         * F(u32),
         * Unknown(u16),
         */
        match ekey.key {
            Key::Enter => {
                if self.coverage != Some(Coverage::Word) {
                    // after coverage_end is set both start and end should propagate
                    // to overlay which handles the rest

                    let original_start = self.coverage_start.unwrap_or(0);

                    let start = cmp::min(self.coverage_start.unwrap_or(0), self.text_location);
                    let mut end = cmp::max(self.coverage_start.unwrap_or(0), self.text_location);

                    if self.coverage == Some(Coverage::Line) {
                        let end_location = self.text_pointer_to_location(end, false);
                        let mut end_length = self.lines.line_length(end_location.row).unwrap_or(0);
                        if end_length > 0 {
                            end_length -= 1;
                        }
                        end = end - end_location.column + end_length;
                    }

                    // when start and end are reversed add 1 to end (so selection takes first marked
                    // char into account)
                    let reverse_add =
                        if self.coverage == Some(Coverage::FromTo) && original_start != start {
                            1
                        } else {
                            0
                        };
                    self.coverage_start = Some(start + reverse_add);
                    self.coverage_end = Some(end + reverse_add);
                }
            }
            Key::Backspace | Key::Delete => {
                // delete current selection
                if self.coverage_start != None {
                    let mut start = cmp::min(self.coverage_start.unwrap(), self.text_location);
                    let end = cmp::max(self.coverage_start.unwrap(), self.text_location);
                    self.text_location = start;
                    while start != end {
                        self.remove_from_buffer();
                        start += 1;
                    }
                    if self.coverage == Some(Coverage::Line) {
                        while self.text_location < self.contents.len()
                            && self.contents[self.text_location] != NEWLINE
                        {
                            self.remove_from_buffer();
                        }
                        if self.text_location < self.contents.len()
                            && self.contents[self.text_location] == NEWLINE
                        {
                            self.remove_from_buffer();
                        }
                    }
                }
            }
            Key::Up | Key::Char(GAME_UP_SHORTCUT) => {
                self.up();
                self.align_buffer_vertical_up(&window_buffer);
            }
            Key::Down | Key::Char(GAME_DOWN_SHORTCUT) => {
                self.down();
                self.align_buffer_vertical_down(&window_buffer, true);
            }
            Key::Right | Key::Char(GAME_RIGHT_SHORTCUT) => {
                self.text_location = cmp::min(self.contents.len() - 1, self.text_location + 1);
                self.align_buffer_horizontal(&window_buffer);
            }
            Key::Left | Key::Char(GAME_LEFT_SHORTCUT) => {
                if self.text_location == 0 {
                    return Event::new();
                }
                self.text_location = cmp::max(0, self.text_location - 1);
                self.align_buffer_horizontal(&window_buffer);
            }
            Key::Home => {
                self.text_location = self.find_start_of_line(self.text_location);
                // scroll buffer to left
                self.view_location.column = 0;
            }
            Key::End => {
                self.text_location = self.find_end_of_line();
                self.align_buffer_horizontal(&window_buffer);
            }
            Key::PageUp => {
                // move down count lines
                for _ in 0..window_buffer.editor_size.rows {
                    self.up();
                }
                self.align_buffer_vertical_up(&window_buffer);
            }
            Key::PageDown => {
                if self.text_location >= self.contents.len() - 1 {
                    return Event::new();
                }
                for _ in 0..window_buffer.editor_size.rows {
                    self.down();
                }
                self.align_buffer_vertical_down(&window_buffer, false);
            }
            Key::Esc => {
                return Event {
                    window_event: Some(WindowEvent::open(ControlType::Menu)),
                    ..Event::new()
                };
            }
            Key::Char(SPACE) => {
                self.set_select_mode(self.coverage);
            }
            _ => {}
        }

        Event::new()
    }

    fn is_selected(&self, index: usize) -> bool {
        if let Some(style) = self.coverage {
            if self.coverage_start == None {
                return false;
            }
            if style == Coverage::Line {
                let start: usize;
                let end: usize;
                if self.text_location < self.coverage_start.unwrap_or(0) {
                    start = self.text_location;
                    end = self.coverage_start.unwrap_or(0);
                } else {
                    start = self.coverage_start.unwrap_or(0);
                    end = self.text_location;
                }

                // get start of current line
                let start_location = self.text_pointer_to_location(start, false);
                let start_highlight = start - start_location.column;

                let end_location = self.text_pointer_to_location(end, false);
                let end_highlight = end - end_location.column
                    + self.lines.line_length(end_location.row).unwrap_or(0);
                return index >= start_highlight && index < end_highlight;
            } else if self.coverage_start != None && self.coverage_end != None {
                return index >= self.coverage_start.unwrap_or(0)
                    && index < self.coverage_end.unwrap_or(0);
            } else {
                return (index >= self.coverage_start.unwrap_or(0) && index <= self.text_location)
                    || (index <= self.coverage_start.unwrap_or(0) && index >= self.text_location);
            }
        }

        if !self.pattern.is_empty() {
            return self
                .matches
                .iter()
                .any(|x| *x <= index && *x + self.pattern.len() > index);
        }

        false
    }

    fn substring(&self, start: usize, end: usize) -> Vec<char> {
        let from = std::cmp::min(start, end);
        let to = std::cmp::max(start, end);

        self.contents[from..std::cmp::min(self.contents.len(), to)].to_vec()
    }

    fn handle_action_only(&mut self, action: Action) {
        match action.action_type {
            ActionType::Insert => {
                self.text_location = action.start_location;
                // TODO: test binary paste
                for c in action.content.chars() {
                    if c == '\u{0}' {
                        log::warn!("Binary data detected. Not pasting.");
                        return;
                    }
                    self.write_to_buffer(c);
                }
            }
            ActionType::Paste => {
                self.text_location = action.start_location;
                // TODO: test binary paste
                for c in action.content.chars() {
                    if c == '\u{0}' {
                        log::warn!("Binary data detected. Not pasting.");
                        return;
                    }
                    self.write_to_buffer(c);
                }
            }
            ActionType::Remove => {
                self.text_location = action.start_location;
                self.remove_from_buffer();
            }
        }
    }

    fn action_do(&mut self, action: Action) {
        // invalidate all actions past current (redo)
        self.action_history.truncate(self.active_action);

        self.handle_action_only(action.clone());
        let mut merged: bool = false;
        if self.active_action > 0 {
            if let Some(previous_action) = self.action_history.get(self.active_action - 1) {
                if previous_action.action_type == ActionType::Insert
                    && (previous_action.start_location + previous_action.len)
                        == action.start_location
                    && !previous_action.content.ends_with(NEWLINE)
                {
                    let mut merged_action = previous_action.clone();
                    merged_action.content += &action.content;
                    merged_action.len = merged_action.content.len();
                    self.action_history[self.active_action - 1] = merged_action;
                    merged = true;
                }
            }
        }

        if !merged {
            // TODO: check if action can be merged
            self.action_history.insert(self.active_action, action);
            self.active_action += 1;

            // forget old actions
            // TODO: store history
            while self.action_history.len() > MAX_ACTION_SIZE {
                self.action_history.remove(0);
            }
        }
    }

    fn get_action_at(&mut self, action_index: usize) -> Option<Action> {
        self.action_history.get(action_index).cloned()
    }

    fn action_redo(&mut self, action_index: usize, window_buffer: Buffer) {
        if action_index >= self.action_history.len() {
            return;
        }

        if let Some(action) = self.get_action_at(action_index) {
            if action.action_type == ActionType::Insert || action.action_type == ActionType::Remove
            {
                self.handle_action_only(action);
                self.active_action += 1;
            }
        }

        self.align_buffer_horizontal(&window_buffer);
        self.align_buffer_vertical_up(&window_buffer);
        self.align_buffer_vertical_down(&window_buffer, false);
    }

    fn action_undo(&mut self, action_index: usize, window_buffer: Buffer) {
        let action_option = self.get_action_at(action_index);
        match action_option {
            Some(action) => {
                match action.action_type {
                    ActionType::Insert | ActionType::Paste => {
                        self.text_location = action.start_location;
                        // TODO: multi-line remove from buffer
                        for _ in 0..action.len {
                            self.remove_from_buffer();
                        }
                        self.align_buffer_horizontal(&window_buffer);
                        self.align_buffer_vertical_up(&window_buffer);
                    }
                    ActionType::Remove => {
                        self.text_location = action.start_location;
                        // TODO: multi-line remove from buffer
                        for c in action.content.chars() {
                            self.write_to_buffer(c);
                        }
                        self.align_buffer_horizontal(&window_buffer);
                        self.align_buffer_vertical_down(&window_buffer, false);
                    }
                }
            }
            None => {
                log::info!("Nonexistent action index.");
            }
        }
    }

    fn render_line_number(
        &self,
        current_location: Location,
        view_location: Location,
        buffer: Buffer,
        size: Size,
    ) {
        let render_location = Location {
            row: current_location.row + buffer.editor_top_left.row - view_location.row,
            column: current_location.column + buffer.editor_top_left.column - view_location.column,
        };

        buffer.write(
            &format!("{:05} ", (current_location.row + 1) % NUMBER_MOD),
            render_location,
            NORMAL_STYLE,
            buffer.editor_top_left,
            size,
        );
    }

    #[allow(dead_code)]
    pub fn get_current_match_location(&self) -> Option<usize> {
        if self.pattern == *"" {
            return None;
        }
        Some(self.matches[self.current_match])
    }

    /**
     * Set current search match index and adjust view so user can see it
     */
    fn set_match_index(&mut self, match_index: usize, window_buffer: Buffer) {
        if match_index >= self.matches.len() {
            return;
        }
        let location = self.matches[match_index];
        let previous_location = self.text_location;
        self.text_location = location;
        self.current_match = match_index;
        if self.text_location > previous_location {
            self.align_buffer_vertical_down(&window_buffer, false);
        } else {
            self.align_buffer_vertical_up(&window_buffer);
        }
    }
}

impl HandleKey for FileBuffer {
    fn handle_key(&mut self, ekey: ExtendedKey, window_buffer: Buffer) -> Event {
        if self.coverage == None {
            self.handle_key_normal(ekey, window_buffer)
        } else {
            self.handle_key_selection(ekey, window_buffer)
        }
    }
}

impl Render for FileBuffer {
    fn render(&self, buffer: &Buffer) {
        let now = Instant::now();
        const SYNTAX_LOOKBACK: usize = 2048;

        let mut current_location: Location =
            Location::new(self.view_location.row, self.view_location.column);

        let view_location = self.view_location;
        let start_of_render = self.find_start_of_render(view_location).unwrap_or(0);

        let start_of_syntax_highlight = if start_of_render < SYNTAX_LOOKBACK {
            0
        } else {
            start_of_render - SYNTAX_LOOKBACK
        };
        let end_of_syntax_highlight = start_of_render + SYNTAX_LOOKBACK;

        let highlights = self.syntax.find_groups(
            &(self.get_slice_string(start_of_syntax_highlight, end_of_syntax_highlight)),
        );

        // TODO: Revise rendering of a box in a larger box (should be an intersection)
        let size = Size {
            rows: buffer.editor_size.rows,
            columns: buffer.editor_size.columns,
        };

        let start_column = view_location.column;

        let number_offset = if self.show_line_number {
            NUMBER_COUNT + 1
        } else {
            0
        };

        while current_location.row < view_location.row + buffer.size.rows {
            let mut anything_rendered = true;

            if self.show_line_number {
                self.render_line_number(current_location, view_location, *buffer, size);
            }

            if let Some(start_of_current_row) = self.find_start_of_render(current_location) {
                for index in start_of_current_row..start_of_current_row + size.columns {
                    if index >= self.contents.len() {
                        anything_rendered = false;
                        break;
                    }

                    let render_location = Location {
                        row: (current_location.row + buffer.editor_top_left.row)
                            .saturating_sub(view_location.row),
                        column: (current_location.column
                            + buffer.editor_top_left.column
                            + number_offset)
                            .saturating_sub(view_location.column),
                    };

                    // iterate until hitting end of buffer or newline
                    let current_character: char = self.contents[index];

                    let mut current_character_str: String = String::from("");
                    current_character_str.push(current_character);

                    let syntax_style: Option<&SyntaxMatch> = highlights.iter().find(|x| {
                        index >= (start_of_syntax_highlight + x.start)
                            && index < (start_of_syntax_highlight + x.end)
                    });

                    let style = if let Some(actual_style) = syntax_style {
                        actual_style.style.clone()
                    } else {
                        NORMAL_STYLE
                    };

                    let char_selected = self.is_selected(index);
                    if self.text_location == index {
                        let selected_style = if self.read_only {
                            NORMAL_STYLE
                        } else if char_selected {
                            style.select_pointer()
                        } else {
                            style.invert()
                        };
                        if current_character == TAB || current_character == NEWLINE {
                            buffer.write(
                                SPACE_STRING,
                                render_location,
                                selected_style,
                                buffer.editor_top_left,
                                size,
                            );

                            if current_character == TAB {
                                let tab_style = if char_selected {
                                    style.invert()
                                } else {
                                    style.clone()
                                };
                                for tab_index in 1..TAB_CHARS_COUNT {
                                    let tab_location = Location::new(
                                        render_location.row,
                                        render_location.column + tab_index,
                                    );
                                    buffer.write(
                                        &String::from(SPACE_STRING),
                                        tab_location,
                                        tab_style.clone(),
                                        buffer.editor_top_left,
                                        size,
                                    );
                                }
                            }
                        } else {
                            buffer.write(
                                &current_character_str,
                                render_location,
                                selected_style,
                                buffer.editor_top_left,
                                size,
                            );
                        }

                        if current_character == TAB {
                            current_location = Location::new(
                                current_location.row,
                                current_location.column + TAB_CHARS_COUNT,
                            );
                        } else if current_character == NEWLINE {
                            // cover empty line - otherwise existing written characters may remain there
                            let next_column = Location {
                                row: render_location.row,
                                column: render_location.column + 1,
                            };
                            buffer.clear_row(next_column, size);
                            current_location =
                                Location::new(current_location.row, current_location.column + 1);
                            break;
                        } else {
                            current_location =
                                Location::new(current_location.row, current_location.column + 1);
                        }
                    } else if current_character == TAB {
                        let tab_style = if char_selected {
                            style.invert()
                        } else {
                            style.clone()
                        };
                        for tab_index in 0..TAB_CHARS_COUNT {
                            let tab_location = Location::new(
                                render_location.row,
                                render_location.column + tab_index,
                            );
                            buffer.write(
                                &String::from(SPACE_STRING),
                                tab_location,
                                tab_style.clone(),
                                buffer.editor_top_left,
                                size,
                            );
                        }
                        current_location = Location::new(
                            current_location.row,
                            current_location.column + TAB_CHARS_COUNT,
                        );
                    } else if current_character == NEWLINE {
                        buffer.clear_row(render_location, size);
                        current_location =
                            Location::new(current_location.row, current_location.column + 1);
                        break;
                    } else {
                        let style = if char_selected { style.invert() } else { style };
                        buffer.write(
                            &current_character_str,
                            render_location,
                            style,
                            buffer.editor_top_left,
                            size,
                        );
                        current_location =
                            Location::new(current_location.row, current_location.column + 1);
                    }
                }
            } else {
                anything_rendered = false;
            }

            let rendered_chars = current_location.column.saturating_sub(start_column);
            if !anything_rendered || rendered_chars < size.columns {
                let start_index = if !anything_rendered {
                    0
                } else {
                    size.columns - rendered_chars
                };

                let current_location = Location::new(
                    current_location.row + buffer.editor_top_left.row,
                    current_location.column + start_index + buffer.editor_top_left.column,
                );
                if self.show_line_number {
                    self.render_line_number(current_location, view_location, *buffer, size);
                }
                for start_index in 0..buffer.size.columns {
                    let current_location = Location::new(
                        current_location.row + buffer.editor_top_left.row,
                        current_location.column + start_index + buffer.editor_top_left.column,
                    );

                    let render_location = Location::new(
                        current_location.row.saturating_sub(view_location.row),
                        (current_location.column + number_offset)
                            .saturating_sub(view_location.column),
                    );

                    buffer.write(
                        &String::from(SPACE_STRING),
                        render_location,
                        NORMAL_STYLE,
                        buffer.editor_top_left,
                        size,
                    );
                }
            }
            current_location = Location::new(current_location.row + 1, view_location.column);
        }
        log::info!(
            "[Performance] Render for filebuffer: {:#?}",
            Instant::now().duration_since(now)
        );
    }
}

impl HandleSearchEvent for FileBuffer {
    fn handle_search_event(&mut self, search_event: SearchEvent, window_buffer: Buffer) -> Event {
        if search_event.pattern != self.pattern {
            self.pattern = search_event.pattern.clone();
            let now = Instant::now();
            self.matches = rabin_karp_search(search_event.pattern, &self.contents, 101);
            log::info!(
                "[Performance] Search time: {:?}",
                Instant::now().duration_since(now)
            );
            self.set_match_index(0, window_buffer);
        } else if !self.matches.is_empty() {
            let mut new_match = self.current_match;
            if search_event.direction == SearchDirection::Forward {
                new_match = (self.current_match + 1) % self.matches.len();
            } else if search_event.direction == SearchDirection::Backward {
                if self.current_match == 0 {
                    new_match = self.matches.len() - 1;
                } else {
                    new_match = (self.current_match - 1) % self.matches.len();
                }
            }
            self.set_match_index(new_match, window_buffer);
        }
        Event::new()
    }
}

impl HandleGotoEvent for FileBuffer {
    fn handle_goto_event(&mut self, goto_event: GotoLineEvent, window_buffer: Buffer) -> Event {
        let line_index = if goto_event.line >= 1 {
            goto_event.line - 1
        } else {
            0
        };
        match self.lines.location(line_index) {
            Some(text_location) => {
                self.text_location = text_location;
                self.align_buffer_vertical_up(&window_buffer);
                self.align_buffer_vertical_down(&window_buffer, false);
            }
            None => {
                // jump to end of file
                let count_lines = self.lines.len();
                if count_lines > 0 {
                    if let Some(last_line_location) =
                        self.lines.location(count_lines.saturating_sub(1))
                    {
                        self.text_location = last_line_location;
                        self.align_buffer_vertical_up(&window_buffer);
                        self.align_buffer_vertical_down(&window_buffer, false);
                    }
                }
            }
        }
        Event::new()
    }
}

impl HandleUndoEvent for FileBuffer {
    fn handle_undo_event(&mut self, undo_event: UndoEvent, window_buffer: Buffer) -> Event {
        match undo_event.action {
            UndoRedoAction::Undo => {
                self.undo(window_buffer);
            }
            UndoRedoAction::Redo => {
                self.redo(window_buffer);
            }
        }
        Event::new()
    }
}

impl HandleSelectEvent for FileBuffer {
    fn handle_select_event(&mut self, select_event: SelectEvent) -> Event {
        let is_first_set = self.coverage != None && self.coverage.unwrap() != select_event.coverage;

        self.coverage = Some(select_event.coverage);
        if is_first_set {
            self.set_select_mode(Some(select_event.coverage));
        }

        if self.coverage_start != None && self.coverage_end != None {
            let buffer = self.substring(
                self.coverage_start.unwrap_or(0),
                self.coverage_end.unwrap_or(0),
            );

            if select_event.action == SelectAction::End {
                self.to_clipboard(buffer);
            }
        }

        Event::new()
    }
}

/**
 * Show help for underlying window
 */
struct HelpOverlay {
    // overlay title
    title: String,
    // top left origin
    location: Location,
    // overlay size
    size: Size,
    // display help text
    file_buffer: FileBuffer,
}

impl HandleKey for HelpOverlay {
    fn handle_key(&mut self, _ekey: ExtendedKey, _window_buffer: Buffer) -> Event {
        Event {
            window_event: Some(WindowEvent::close(ControlType::HelpOverlay, None)),
            ..Event::new()
        }
    }
}

impl Render for HelpOverlay {
    fn render(&self, buffer: &Buffer) {
        self.render_window(buffer);

        let modified_buffer = Buffer {
            editor_top_left: Location {
                row: self.location.row + 1,
                column: self.location.column + 1,
            },
            editor_size: Size::new(self.size.rows - 2, self.size.columns - 2),
            ..*buffer
        };
        self.file_buffer.render(&modified_buffer);
    }
}

impl Window for HelpOverlay {
    fn get_origin(&self) -> Location {
        self.location
    }

    fn get_size(&self) -> Size {
        self.size
    }

    fn get_title(&self) -> String {
        self.title.clone()
    }
}

/**
 * Undo/Redo overlay allows undoing/redoing of recent actions
 *
 * Originally this was done as there is no easy way in shell to detect ctrl+shift+z shortcut
 *
 * Keys do the following in undo/redo overlay:
 * Tab - Switch between Undo/Redo
 * Space,Enter - Apply selected action
 *
 * Toggle mode with Tab listed shortcuts
 */
struct UndoRedoOverlay {
    // overlay title
    title: String,
    // overlay size
    size: Size,

    // selected action
    action: UndoRedoAction,
}

impl HandleKey for UndoRedoOverlay {
    fn handle_key(&mut self, ekey: ExtendedKey, _window_buffer: Buffer) -> Event {
        match ekey.key {
            Key::Esc => {
                // close overlay
                return Event {
                    window_event: Some(WindowEvent::close(ControlType::UndoRedoOverlay, None)),
                    ..Event::new()
                };
            }
            Key::Enter | Key::Char(SPACE) => {
                return Event {
                    undo_event: Some(UndoEvent {
                        action: self.action,
                    }),
                    window_event: Some(WindowEvent::close(ControlType::UndoRedoOverlay, None)),
                    ..Event::new()
                };
            }
            HELP_SHORTCUT => {
                return Event {
                    window_event: Some(WindowEvent::open(ControlType::HelpOverlay)),
                    ..Event::new()
                };
            }
            Key::Tab => {
                let count = UndoRedoAction::iter().count();
                let index = UndoRedoAction::iter()
                    .position(|&cov| cov == self.action)
                    .unwrap_or(0);
                self.action = *UndoRedoAction::iter()
                    .nth((index + 1) % count)
                    .unwrap_or(&UndoRedoAction::Undo);
                return Event { ..Event::new() };
            }
            _ => {
                // skip unknown chars
            }
        }
        Event::new()
    }
}

impl Render for UndoRedoOverlay {
    fn render(&self, buffer: &Buffer) {
        self.render_window(buffer);

        let mode = String::from("Mode: ");
        let status = format!("{:?}", self.action);

        let padding_top_left = [1, 1];
        let top_left = self.get_origin();
        let row = top_left.row + WINDOW_BORDER[0];
        let column = top_left.column + WINDOW_BORDER[1] + padding_top_left[1];
        let location = Location::new(row, column);
        buffer.write_direct(&mode, location, NORMAL_STYLE);
        buffer.write_direct(
            &status,
            Location::new(row, column + mode.len()),
            INVERSE_STYLE,
        );
    }
}

impl Window for UndoRedoOverlay {
    fn get_origin(&self) -> Location {
        Location::new(0, 0)
    }

    fn get_size(&self) -> Size {
        self.size
    }

    fn get_title(&self) -> String {
        self.title.clone()
    }
}

/**
 * Selection overlay manages selection in the underlying active file buffer
 *
 * Keys do the following in selection cover buffer:
 * Left,Down,Up,Right - move around
 * PageUp,PageDown,Home,End - move around
 *
 * Space marks start of selection (in case From-To selection is marked)
 * Enter takes currently highlighted selection and
 * executes an action on selection (currently only copy to clipboard)
 *
 * Toggle mode with Tab, Shift+Tab or listed shortcuts
 */
struct SelectionOverlay {
    coverage: Coverage,
    // overlay title
    title: String,
    // overlay size
    size: Size,
}

impl SelectionOverlay {}

impl HandleKey for SelectionOverlay {
    fn handle_key(&mut self, ekey: ExtendedKey, _window_buffer: Buffer) -> Event {
        match ekey.key {
            Key::Esc => {
                return Event {
                    window_event: Some(WindowEvent::close(ControlType::SelectionOverlay, None)),
                    ..Event::key(ekey)
                };
            }
            HELP_SHORTCUT => {
                return Event {
                    window_event: Some(WindowEvent::open(ControlType::HelpOverlay)),
                    ..Event::new()
                };
            }
            Key::Enter => {
                // TODO: take selection and paste to clipboard
                return Event {
                    bubble_down: true,
                    select_event: Some(SelectEvent {
                        action: SelectAction::End,
                        coverage: self.coverage,
                    }),
                    window_event: Some(WindowEvent::close(ControlType::SelectionOverlay, None)),
                    ..Event::key(ekey)
                };
            }
            Key::Backspace | Key::Delete => {
                return Event {
                    window_event: Some(WindowEvent::close(ControlType::SelectionOverlay, None)),
                    bubble_down: true,
                    ..Event::key(ekey)
                };
            }
            Key::Home | Key::End => {
                return Event {
                    bubble_down: true,
                    ..Event::key(ekey)
                };
            }
            Key::Up
            | Key::Down
            | Key::Left
            | Key::Right
            | Key::PageUp
            | Key::PageDown
            | Key::Char(GAME_UP_SHORTCUT)
            | Key::Char(GAME_DOWN_SHORTCUT)
            | Key::Char(GAME_LEFT_SHORTCUT)
            | Key::Char(GAME_RIGHT_SHORTCUT) => {
                return Event {
                    bubble_down: true,
                    ..Event::key(ekey)
                };
            }
            Key::Tab => {
                let count = Coverage::iter().count();
                let index = Coverage::iter()
                    .position(|&cov| cov == self.coverage)
                    .unwrap_or(0);
                self.coverage = *Coverage::iter()
                    .nth((index + 1) % count)
                    .unwrap_or(&Coverage::FromTo);
                return Event {
                    bubble_down: true,
                    ..Event::key(ekey)
                };
            }
            Key::Char(input_char) => {
                if input_char == SPACE {
                    return Event {
                        bubble_down: true,
                        select_event: Some(SelectEvent {
                            action: SelectAction::Start,
                            coverage: self.coverage,
                        }),
                        ..Event::key(ekey)
                    };
                }
            }
            _ => {
                // skip unknown chars
            }
        }
        Event {
            window_event: Some(WindowEvent::close(ControlType::SelectionOverlay, None)),
            ..Event::key(ekey)
        }
    }
}

impl Render for SelectionOverlay {
    fn render(&self, buffer: &Buffer) {
        self.render_window(buffer);

        let mode = String::from("Mode: ");
        let status = format!("{:?}", self.coverage);

        let padding_top_left = [1, 1];
        let top_left = self.get_origin();
        let row = top_left.row + WINDOW_BORDER[0];
        let column = top_left.column + WINDOW_BORDER[1] + padding_top_left[1];
        let location = Location::new(row, column);
        buffer.write_direct(&mode, location, NORMAL_STYLE);
        buffer.write_direct(
            &status,
            Location::new(row, column + mode.len()),
            INVERSE_STYLE,
        );
    }
}

impl Window for SelectionOverlay {
    fn get_origin(&self) -> Location {
        Location::new(0, 0)
    }

    fn get_size(&self) -> Size {
        self.size
    }

    fn get_title(&self) -> String {
        self.title.clone()
    }
}

/**
 * Search overlay allows searching through current buffer
 *
 * Enter - goes to next search result
 * Arrow Up/Left, Arrow Down/Right navigates results
 *
 * Input characters are added or removed via (del, backspace) to search.
 *
 * Can be fired up with CTRL+f by default
 */
#[derive(Clone, Debug)]
struct SearchOverlay {
    // overlay title
    title: String,
    // overlay size
    size: Size,

    // search pattern
    // once pattern is entered it can no longer be changed (restart search by CTRL+f to search anew)
    pattern_read_only: bool,
    pattern_location: usize,
    pattern: String,

    // if filter mode search overlay should filter underlying content
    filter_mode: bool,
}

impl SearchOverlay {
    fn create_close_event(&self) -> Event {
        Event {
            window_event: Some(WindowEvent::close(ControlType::SearchOverlay, None)),
            search_event: Some(SearchEvent {
                direction: SearchDirection::Forward,
                pattern: "".to_string(),
            }),
            ..Event::new()
        }
    }
}

impl HandleKey for SearchOverlay {
    fn handle_key(&mut self, ekey: ExtendedKey, _window_buffer: Buffer) -> Event {
        match ekey.key {
            Key::Esc => {
                return self.create_close_event();
            }
            Key::Enter => {
                self.pattern_read_only = true;

                // if this overlay behaves as a filter just close overlay after filtering
                let window_event = if self.filter_mode {
                    Some(WindowEvent::close(ControlType::SearchOverlay, None))
                } else {
                    None
                };

                // TODO: take selection and paste to clipboard
                return Event {
                    search_event: Some(SearchEvent {
                        direction: SearchDirection::Forward,
                        pattern: self.pattern.clone(),
                    }),
                    window_event,
                    ..Event::new()
                };
            }
            Key::Tab => {
                if !self.pattern_read_only {
                    for _ in 0..4 {
                        self.pattern.push(SPACE);
                    }
                    self.pattern_location += 4;
                }
                return Event::new();
            }
            HELP_SHORTCUT => {
                return Event {
                    window_event: Some(WindowEvent::open(ControlType::HelpOverlay)),
                    ..Event::new()
                };
            }
            Key::Char(c) => {
                if ekey.modifiers.ctrl && c == CTRL_MENU_SHORTCUT {
                    return self.create_close_event();
                } else if ekey.modifiers.ctrl && c == CTRL_SEARCH_SHORTCUT {
                    self.pattern_read_only = false;
                    self.pattern_location = 0;
                    self.pattern = "".to_string();
                } else if !self.pattern_read_only {
                    self.pattern.insert(self.pattern_location, c);
                    self.pattern_location += 1;
                } else if c == SEARCH_FORWARD_SHORTCUT {
                    return Event {
                        search_event: Some(SearchEvent {
                            direction: SearchDirection::Forward,
                            pattern: self.pattern.clone(),
                        }),
                        ..Event::new()
                    };
                } else if c == SEARCH_BACKWARD_SHORTCUT {
                    return Event {
                        search_event: Some(SearchEvent {
                            direction: SearchDirection::Backward,
                            pattern: self.pattern.clone(),
                        }),
                        ..Event::new()
                    };
                }
                return Event::new();
            }
            Key::Backspace => {
                if !self.pattern_read_only && self.pattern_location > 0 && !self.pattern.is_empty()
                {
                    self.pattern.remove(self.pattern_location - 1);
                    self.pattern_location -= 1;
                }
                return Event::new();
            }
            Key::Delete => {
                if !self.pattern_read_only && self.pattern_location < self.pattern.len() {
                    self.pattern.remove(self.pattern_location);
                }
                return Event::new();
            }
            Key::Left => {
                if !self.pattern_read_only && self.pattern_location > 0 {
                    self.pattern_location -= 1;
                }
                return Event::new();
            }
            Key::Right => {
                if !self.pattern_read_only {
                    self.pattern_location = cmp::min(self.pattern.len(), self.pattern_location + 1);
                }
                return Event::new();
            }
            Key::Up => {
                // TODO: search
            }
            Key::Down => {
                // TODO: search
            }
            Key::Home => {
                if !self.pattern_read_only {
                    self.pattern_location = 0;
                }
                return Event::new();
            }
            Key::End => {
                if !self.pattern_read_only {
                    self.pattern_location = self.pattern.len();
                }
                return Event::new();
            }
            _ => {
                return Event {
                    window_event: Some(WindowEvent::close(ControlType::SearchOverlay, None)),
                    ..Event::new()
                };
            }
        }
        Event {
            window_event: Some(WindowEvent::close(ControlType::SearchOverlay, None)),
            ..Event::new()
        }
    }
}

impl Render for SearchOverlay {
    fn render(&self, buffer: &Buffer) {
        self.render_window(buffer);
        let label = format!("{}: ", self.title);

        let padding_top_left = [1, 1];
        let top_left = self.get_origin();
        let row = top_left.row + WINDOW_BORDER[0];
        let column = top_left.column + WINDOW_BORDER[1] + padding_top_left[1];
        let label_location = Location::new(row, column);
        buffer.write_direct(&label, label_location, NORMAL_STYLE);
        let mut extended_pattern = self.pattern.clone();
        extended_pattern.push(SPACE);
        for (i, ch) in extended_pattern.chars().enumerate() {
            let location = Location::new(row, column + i + label.len());
            let style = if i == self.pattern_location {
                INVERSE_STYLE
            } else {
                NORMAL_STYLE
            };
            buffer.write_direct(&ch.to_string(), location, style);
        }
    }
}

impl Window for SearchOverlay {
    fn get_origin(&self) -> Location {
        Location::new(0, 0)
    }

    fn get_size(&self) -> Size {
        self.size
    }

    fn get_title(&self) -> String {
        self.title.clone()
    }
}

/**
 * Menu represents an application menu that can be navigated with either
 * - tab/shift+tab
 * - arrow keys + enter
 * - shortcuts (listed next to menu entry)
 *
 * Menu choices reflect assigned shortcuts on keyboard
 */
struct Menu {
    items: LinkedList<MenuItem>,
    selected_index: usize,

    // window fields
    title: String,
    origin: Location,
    size: Size,
}

type MenuItemCallback = fn(item: &MenuItem) -> Event;

fn new_callback(_item: &MenuItem) -> Event {
    Event {
        create_file_event: Some(CreateFileEvent {}),
        window_event: Some(WindowEvent::close(ControlType::Menu, None)),
        ..Event::new()
    }
}

fn open_callback(_item: &MenuItem) -> Event {
    Event {
        window_event: Some(WindowEvent::open(ControlType::OpenFileMenu)),
        ..Event::new()
    }
}

fn open_buffer_callback(_item: &MenuItem) -> Event {
    // TODO: open buffers
    Event {
        window_event: Some(WindowEvent::open(ControlType::FileBuffer)),
        ..Event::new()
    }
}

fn save_callback(_item: &MenuItem) -> Event {
    Event {
        window_event: Some(WindowEvent::open(ControlType::YesNoDialog)),
        ..Event::new()
    }
}

fn save_as_callback(_item: &MenuItem) -> Event {
    Event {
        window_event: Some(WindowEvent::open_dialog(
            ControlType::InputDialog,
            DialogType::Save,
        )),
        ..Event::new()
    }
}

fn goto_line_callback(_item: &MenuItem) -> Event {
    Event {
        window_event: Some(WindowEvent::close(ControlType::InputDialog, None)),
        ..Event::new()
    }
}

fn save_as_close_callback(_item: &MenuItem) -> Event {
    Event {
        window_event: Some(WindowEvent::close(ControlType::InputDialog, None)),
        ..Event::new()
    }
}

fn save_file_buffer_callback(item: &MenuItem) -> Event {
    log::info!("Saving to file {}", item.file_path);
    match OpenOptions::new()
        .create(true)
        .write(true)
        .open(&item.file_path)
    {
        Err(reason) => {
            // TODO: show dialog with the problem
            log::warn!("Failed opening file: {} Reason: {}", item.file_path, reason);
        }
        Ok(mut file) => {
            log::warn!("Attempt save to file: {}", item.file_path);
            const BUFFER_SIZE: usize = 1024;
            let mut buffer: [u8; BUFFER_SIZE] = [0; BUFFER_SIZE];
            // buffer one is used for last 1024 byees
            let mut buffer_one: [u8; 1] = [0];
            let mut i = 0;
            while i < item.contents.len() {
                let start = i;
                if start + BUFFER_SIZE < item.contents.len() {
                    for j in start..start + BUFFER_SIZE {
                        if j >= item.contents.len() {
                            buffer[j % BUFFER_SIZE] = 0_u8;
                            continue;
                        }
                        if item.contents[j % BUFFER_SIZE] == '\u{0}' {
                            panic!("NUL byte encountered! Saving failed...");
                        }
                        buffer[j % BUFFER_SIZE] = item.contents[j] as u8;
                    }
                    // write buffer to file
                    match file.write(&buffer) {
                        Ok(result) => {
                            log::info!("Written {} bytes", result);
                        }
                        Err(e) => {
                            log::warn!("Failed writing bytes: {:?}", e);
                        }
                    }
                } else {
                    for j in start..start + BUFFER_SIZE {
                        if j >= item.contents.len() {
                            continue;
                        }
                        if item.contents[j % BUFFER_SIZE] == '\u{0}' {
                            // NUL bytes signify it's a binary file
                            continue;
                        }
                        buffer_one[0] = item.contents[j] as u8;

                        // write buffer to file
                        match file.write(&buffer_one) {
                            Ok(_) => { /* noop */ }
                            Err(e) => {
                                log::warn!("Failed writing to file: {:?}", e);
                                break;
                            }
                        }
                    }
                }
                i += BUFFER_SIZE;
            }
            match file.set_len(item.contents.len() as u64) {
                Ok(_) => { /* noop */ }
                Err(e) => {
                    log::warn!("Failed truncating file to proper length: {:?}", e);
                }
            }
        }
    };
    Event {
        window_event: Some(WindowEvent::close(ControlType::YesNoDialog, None)),
        ..Event::new()
    }
}

fn save_close_callback(_item: &MenuItem) -> Event {
    Event {
        window_event: Some(WindowEvent::close(ControlType::YesNoDialog, None)),
        ..Event::new()
    }
}

fn exit_callback(_item: &MenuItem) -> Event {
    Event {
        exit_event: Some(ExitEvent {}),
        ..Event::new()
    }
}

struct MenuItem {
    shortcut: Option<char>,
    selected: bool,
    callback: MenuItemCallback,

    // parameters
    file_path: String,
    contents: Vec<char>,

    // window fields
    title: String,
}

impl MenuItem {
    pub fn new(
        title: String,
        shortcut: Option<char>,
        selected: bool,
        callback: MenuItemCallback,
    ) -> Self {
        Self {
            shortcut,
            selected,
            callback,
            file_path: String::from(""),
            contents: Vec::new(),
            title,
        }
    }

    pub fn from(menu_item: &MenuItem) -> Self {
        Self {
            shortcut: menu_item.shortcut,
            selected: menu_item.selected,
            callback: menu_item.callback,
            file_path: menu_item.file_path.clone(),
            contents: menu_item.contents.clone(),
            title: menu_item.title.clone(),
        }
    }
}

impl Menu {
    fn down(&mut self) {
        self.selected_index = (self.selected_index + 1) % self.items.len();
        for (item_index, item) in self.items.iter_mut().enumerate() {
            item.selected = item_index == self.selected_index;
        }
    }
}

impl HandleKey for Menu {
    fn handle_key(&mut self, ekey: ExtendedKey, _window_buffer: Buffer) -> Event {
        match ekey.key {
            Key::Esc => {
                return Event {
                    window_event: Some(WindowEvent::close(ControlType::Menu, None)),
                    ..Event::new()
                };
            }
            Key::Enter | Key::Char(SPACE) => {
                if let Some(selected_item) = self.items.iter().nth(self.selected_index) {
                    return (selected_item.callback)(selected_item);
                }
            }
            Key::Up => {
                if self.selected_index > 0 {
                    self.selected_index = (self.selected_index - 1) % self.items.len();
                } else {
                    self.selected_index = self.items.len() - 1;
                }
                for (item_index, item) in self.items.iter_mut().enumerate() {
                    item.selected = item_index == self.selected_index;
                }
                return Event::new();
            }
            Key::Down => {
                self.down();
                return Event::new();
            }
            // TODO: Shift+Tab currently not supported
            Key::Tab => {
                self.down();
                return Event::new();
            }
            HELP_SHORTCUT => {
                return Event {
                    window_event: Some(WindowEvent::open(ControlType::HelpOverlay)),
                    ..Event::new()
                };
            }
            Key::Char(input_char) => {
                if ekey.modifiers.ctrl && input_char == CTRL_MENU_SHORTCUT {
                    return Event {
                        window_event: Some(WindowEvent::close(ControlType::Menu, None)),
                        ..Event::new()
                    };
                }

                // check if any shortcut hit
                if let Some(item) = self
                    .items
                    .iter()
                    .find(|item| item.shortcut == Some(input_char))
                {
                    return (item.callback)(item);
                }
            }
            _ => {
                // skip unknown chars
            }
        }
        Event::new()
    }
}

/**
 * All UI controls should implement render to allow for visual representation
 */
impl Render for Menu {
    fn render(&self, buffer: &Buffer) {
        self.render_window(buffer);

        for (item_index, item) in self.items.iter().enumerate() {
            let padding_top_left = [1, 1];
            let row_spacing = 1;

            let top_left = self.get_origin();
            let row = top_left.row + WINDOW_BORDER[0] + item_index + row_spacing * (item_index + 1);
            let column = top_left.column + WINDOW_BORDER[1] + padding_top_left[1];
            let location = Location::new(row, column);
            let shortcut_location = Location::new(row, column + item.title.len() + 1);

            let style = if self.selected_index == item_index {
                INVERSE_STYLE
            } else {
                NORMAL_STYLE
            };
            buffer.write_direct(&item.title, location, style);
            if let Some(shortcut) = item.shortcut {
                buffer.write_direct(&format!("({})", shortcut), shortcut_location, NORMAL_STYLE);
            }
        }
    }
}

impl Window for Menu {
    fn get_origin(&self) -> Location {
        self.origin
    }

    fn get_size(&self) -> Size {
        self.size
    }

    fn get_title(&self) -> String {
        self.title.clone()
    }
}

struct YesNoDialog {
    text: String,
    menu_items: LinkedList<MenuItem>,
    selected_index: usize,

    // window fields
    title: String,
    origin: Location,
    size: Size,
}

impl Render for YesNoDialog {
    fn render(&self, buffer: &Buffer) {
        self.render_window(buffer);

        let top_left = self.get_origin();
        let size = self.get_size();
        // render text
        let text_top_left = Location {
            row: top_left.row + WINDOW_BORDER[0],
            column: top_left.column + WINDOW_BORDER[1],
        };

        const SKIP_BOTTOM: usize = 3;
        let text_size = Size {
            rows: size.rows - 2 * WINDOW_BORDER[0] - SKIP_BOTTOM,
            columns: size.columns - 2 * WINDOW_BORDER[1] - 2,
        };

        let mut row_index = 0;
        let mut text_index = 0;
        while text_index < self.text.len() {
            let cut_string: String = self
                .text
                .graphemes(true)
                .skip(text_index)
                .take(text_size.columns)
                .collect();

            let location = Location {
                row: text_top_left.row + WINDOW_BORDER[0] + row_index + 1,
                column: text_top_left.column + WINDOW_BORDER[1],
            };

            buffer.write_direct(&cut_string, location, NORMAL_STYLE);
            row_index += 1;
            text_index += text_size.columns;
        }

        let count = self.menu_items.len();
        let single_width = (size.columns - 2 * WINDOW_BORDER[1]) / count;
        for (menu_item_index, menu_item) in self.menu_items.iter().enumerate() {
            let cut_string: String = menu_item.title.graphemes(true).take(single_width).collect();
            let location = Location {
                row: top_left.row + size.rows - WINDOW_BORDER[0] - 1,
                column: top_left.column
                    + WINDOW_BORDER[1]
                    + single_width * menu_item_index
                    + (single_width - cut_string.len()) / 2,
            };
            let style = if menu_item.selected {
                INVERSE_STYLE
            } else {
                NORMAL_STYLE
            };
            buffer.write_direct(&cut_string, location, style);
        }
    }
}

impl HandleKey for YesNoDialog {
    fn handle_key(&mut self, ekey: ExtendedKey, _window_buffer: Buffer) -> Event {
        match ekey.key {
            Key::Esc => {
                return Event {
                    window_event: Some(WindowEvent::close(ControlType::YesNoDialog, None)),
                    ..Event::new()
                };
            }
            Key::Enter => {
                if let Some(selected_item) = self.menu_items.iter().nth(self.selected_index) {
                    return (selected_item.callback)(selected_item);
                }
            }
            Key::Left => {
                if self.selected_index > 0 {
                    self.selected_index = (self.selected_index - 1) % self.menu_items.len();
                } else {
                    self.selected_index = self.menu_items.len() - 1;
                }
                for (item_index, item) in self.menu_items.iter_mut().enumerate() {
                    item.selected = item_index == self.selected_index;
                }
            }
            Key::Right => {
                self.selected_index = (self.selected_index + 1) % self.menu_items.len();
                for (item_index, item) in self.menu_items.iter_mut().enumerate() {
                    item.selected = item_index == self.selected_index;
                }
            }
            // TODO: Shift+Tab currently not supported
            Key::Tab => {
                // self.down();
            }
            _ => {
                // skip unknown chars
            }
        }

        Event::new()
    }
}

impl Window for YesNoDialog {
    fn get_origin(&self) -> Location {
        self.origin
    }

    fn get_size(&self) -> Size {
        self.size
    }

    fn get_title(&self) -> String {
        self.title.clone()
    }
}

struct InputDialog {
    text: String,
    input: String,
    input_location: usize,
    input_validation_regex: Regex,

    menu_items: LinkedList<MenuItem>,
    selected_index: usize,
    dialog_type: DialogType,

    // window fields
    title: String,
    origin: Location,
    size: Size,
}

impl Render for InputDialog {
    fn render(&self, buffer: &Buffer) {
        self.render_window(buffer);

        let top_left = self.get_origin();
        let size = self.get_size();
        // render text
        let text_top_left = Location {
            row: top_left.row + WINDOW_BORDER[0],
            column: top_left.column + WINDOW_BORDER[1],
        };

        let row_index = 0;
        let all_text = format!(
            "{} {} ",
            self.text,
            if self.input.is_empty() {
                " "
            } else {
                &self.input
            }
        );

        for (text_index, c) in all_text.chars().enumerate() {
            let location = Location {
                row: text_top_left.row + WINDOW_BORDER[0] + row_index + 1,
                column: text_top_left.column + WINDOW_BORDER[1] + text_index,
            };

            let style = if text_index == self.text.len() + self.input_location + 1 {
                INVERSE_STYLE
            } else {
                NORMAL_STYLE
            };
            buffer.write_direct(&c.to_string(), location, style);
        }

        let count = self.menu_items.len();
        let single_width = (size.columns - 2 * WINDOW_BORDER[1]) / count;
        for (menu_item_index, menu_item) in self.menu_items.iter().enumerate() {
            let cut_string: String = menu_item.title.graphemes(true).take(single_width).collect();
            let location = Location {
                row: top_left.row + size.rows - WINDOW_BORDER[0] - 1,
                column: top_left.column
                    + WINDOW_BORDER[1]
                    + single_width * menu_item_index
                    + (single_width - cut_string.len()) / 2,
            };
            let style = if menu_item.selected {
                INVERSE_STYLE
            } else {
                NORMAL_STYLE
            };
            buffer.write_direct(&cut_string, location, style);
        }
    }
}

impl HandleKey for InputDialog {
    fn handle_key(&mut self, ekey: ExtendedKey, _window_buffer: Buffer) -> Event {
        match ekey.key {
            Key::Tab => {
                let mut modified_input: String = self.input.clone();
                for _ in 0..4 {
                    modified_input.push(SPACE);
                }

                if !self.input_validation_regex.is_match(&modified_input) {
                    return Event::new();
                } else {
                    for _ in 0..4 {
                        self.input.push(SPACE);
                    }
                    self.input_location += 4;
                }
            }
            Key::Home => {
                self.input_location = 0;
            }
            Key::End => {
                self.input_location = self.input.len();
            }
            Key::Char(c) => {
                let mut modified_input: String = self.input.clone();
                modified_input.insert(self.input_location, c);
                if !self.input_validation_regex.is_match(&modified_input) {
                    return Event::new();
                } else {
                    self.input.insert(self.input_location, c);
                    self.input_location += 1;
                }
            }
            Key::Backspace => {
                if self.input_location > 0 && !self.input.is_empty() {
                    self.input.remove(self.input_location - 1);
                    self.input_location -= 1;
                }
            }
            Key::Delete => {
                if self.input_location < self.input.len() {
                    self.input.remove(self.input_location);
                }
            }
            Key::Esc => {
                return Event {
                    window_event: Some(WindowEvent::close(ControlType::InputDialog, None)),
                    ..Event::new()
                };
            }
            Key::Enter => {
                if self.dialog_type == DialogType::Save {
                    if let Some(selected_item) = self.menu_items.iter().nth(self.selected_index) {
                        // run save file callback on modified item with file_path from this InputDialog
                        let mut item_copy = MenuItem::from(selected_item);
                        item_copy.file_path = self.input.clone();
                        return (selected_item.callback)(&item_copy);
                    }
                }

                let mut gotoline_event = None;
                if self.dialog_type == DialogType::Goto && !self.input.is_empty() {
                    match self.input.parse() {
                        Ok(result) => gotoline_event = Some(GotoLineEvent { line: result }),
                        Err(e) => {
                            // this can happen due to overflows
                            log::warn!("Failed parsing input: {:?}", e);
                        }
                    }
                }

                if let Some(selected_item) = self.menu_items.iter_mut().nth(self.selected_index) {
                    selected_item.file_path = self.input.clone();
                    return Event {
                        window_event: Some(WindowEvent::close(ControlType::InputDialog, None)),
                        gotoline_event,
                        ..Event::new()
                    };
                }

                return Event {
                    window_event: Some(WindowEvent::close(ControlType::InputDialog, None)),
                    gotoline_event,
                    ..Event::new()
                };
            }
            Key::Left => {
                if self.selected_index > 0 {
                    self.selected_index = (self.selected_index - 1) % self.menu_items.len();
                } else {
                    self.selected_index = self.menu_items.len() - 1;
                }
                for (item_index, item) in self.menu_items.iter_mut().enumerate() {
                    item.selected = item_index == self.selected_index;
                }
            }
            Key::Right => {
                self.selected_index = (self.selected_index + 1) % self.menu_items.len();
                for (item_index, item) in self.menu_items.iter_mut().enumerate() {
                    item.selected = item_index == self.selected_index;
                }
            }
            _ => {
                // skip unknown chars
            }
        }

        Event::new()
    }
}

impl Window for InputDialog {
    fn get_origin(&self) -> Location {
        self.origin
    }

    fn get_size(&self) -> Size {
        self.size
    }

    fn get_title(&self) -> String {
        self.title.clone()
    }
}

/**
 * Mode of operation for open file menu
 *
 * Either displays buffers or files in file system
 */
#[derive(Copy, Clone, Debug, PartialEq)]
enum OpenMenuMode {
    // show files
    File,
    // show buffers
    Buffer,
}

/**
 * OpenFileMenu represents a NC-like full screen file open control
 * - tab/shift+tab
 * - arrow keys + enter (enter on folder expands/retracts folder)
 * - shortcuts (listed next to menu entry)
 *
 * Menu choices reflect assigned shortcuts on keyboard
 */
struct OpenFileMenu {
    mode: OpenMenuMode,

    view_start: usize,
    original_items: Vec<FileItem>,
    items: Vec<FileItem>,
    // selected index is an offset from start to currently selected item
    // it points to current state of tree - if tree inadvertently changes index might be incorrect
    // but currently all changes are local to state at index
    selected_index: usize,
    // total expanded tree items
    total_items: usize,

    last_loaded_dir: Option<PathBuf>,
    pattern: String,

    // window fields
    title: String,
    size: Size,
}

struct PathBufItem {
    depth: usize,
    pathbuf: PathBuf,
}

impl OpenFileMenu {
    /**
     * Load first N-levels directory and files under given path
     */
    fn load_directory(&self, dir: PathBuf, levels: usize) -> std::io::Result<Vec<FileItem>> {
        let mut children: Vec<FileItem> = Vec::new();

        let mut dirs_to_process: Vec<PathBufItem> = Vec::new();

        for entry in fs::read_dir(dir.as_path())? {
            match entry {
                Ok(dir) => {
                    dirs_to_process.push(PathBufItem {
                        depth: 0,
                        pathbuf: dir.path().to_path_buf(),
                    });
                }
                Err(e) => {
                    log::warn!("Failed reading dir: {:?} reason: {:?}", dir, e);
                }
            }
        }

        dirs_to_process.sort_by(|l, r| {
            if l.pathbuf.is_dir() == r.pathbuf.is_dir() {
                l.pathbuf
                    .to_str()
                    .unwrap()
                    .partial_cmp(r.pathbuf.to_str().unwrap())
                    .unwrap()
            } else if l.pathbuf.is_dir() && !r.pathbuf.is_dir() {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        });

        while !dirs_to_process.is_empty() {
            let pathbuf_item = dirs_to_process.remove(0);
            if pathbuf_item.depth >= levels {
                continue;
            }

            let is_dir = pathbuf_item.pathbuf.is_dir();
            if !self.pattern.is_empty()
                && !is_dir
                && !String::from(pathbuf_item.pathbuf.to_str().unwrap_or(""))
                    .contains(&self.pattern)
            {
                continue;
            }

            if !self.pattern.is_empty() && is_dir {
            } else {
                let visible = if !self.pattern.is_empty() {
                    true
                } else {
                    pathbuf_item.depth == 0
                };
                children.push(FileItem {
                    is_dir,
                    path: pathbuf_item.pathbuf.clone(),
                    expanded: false,
                    depth: pathbuf_item.depth,
                    visible,
                });
            }

            if pathbuf_item.depth < LOAD_DIRECTORY_DEPTH && is_dir {
                let mut subdirs_to_process: Vec<PathBufItem> = Vec::new();
                for entry in fs::read_dir(pathbuf_item.pathbuf.as_path())? {
                    match entry {
                        Ok(dir) => {
                            subdirs_to_process.push(PathBufItem {
                                depth: pathbuf_item.depth + 1,
                                pathbuf: dir.path().to_path_buf(),
                            });
                        }
                        Err(e) => {
                            log::warn!("Failed reading dir: {:?} reason: {:?}", dir, e);
                        }
                    }
                }

                subdirs_to_process.sort_by(|l, r| {
                    if l.pathbuf.is_dir() == r.pathbuf.is_dir() {
                        l.pathbuf
                            .to_str()
                            .unwrap()
                            .partial_cmp(r.pathbuf.to_str().unwrap())
                            .unwrap()
                    } else if l.pathbuf.is_dir() && !r.pathbuf.is_dir() {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    }
                });

                for (entry_index, subdir) in subdirs_to_process.into_iter().enumerate() {
                    dirs_to_process.insert(entry_index, subdir);
                }
            }
        }

        // children.sort_unstable_by(compare_items);
        Ok(children)
    }

    fn load_buffers(&self, buffers: &[FileBuffer]) -> std::io::Result<Vec<FileItem>> {
        let mut loaded_buffers: Vec<FileItem> = Vec::new();
        for buffer in buffers {
            if !self.pattern.is_empty()
                && !String::from(buffer.file_path.as_str()).contains(&self.pattern)
            {
                continue;
            }
            loaded_buffers.push(FileItem {
                path: PathBuf::from(buffer.file_path.clone()),
                ..FileItem::new_buffer(buffer.buffer_id)
            });
        }
        Ok(loaded_buffers)
    }

    fn reload_buffers(&mut self) {
        self.items = Vec::new();
        for original_item in &self.original_items[..] {
            if !self.pattern.is_empty()
                && !String::from(original_item.path.to_str().unwrap_or("")).contains(&self.pattern)
            {
                continue;
            }

            if !self.pattern.is_empty() && original_item.is_dir {
                continue;
            }

            self.items.push(original_item.clone());
        }
    }

    fn height(&self, window_buffer: &Buffer) -> usize {
        window_buffer.size.rows - 2
    }

    /**
     * Given a new position calculate start position to display menu from
     */
    fn align_vertical(&mut self, updated_position: usize, window_buffer: &Buffer) {
        let mut count_invisible = 0;
        for item in &self.items[0..updated_position] {
            if !item.visible {
                count_invisible += 1;
            }
        }

        let new_position = updated_position - count_invisible;

        let height = self.height(window_buffer);
        if new_position >= self.view_start && new_position < (self.view_start + height) {
            // all good
            return;
        }

        // try moving slider forward
        let mut new_view_start = self.view_start;
        while (new_view_start + height) <= new_position {
            new_view_start += height;
        }
        if new_position >= new_view_start && new_position < (new_view_start + height) {
            self.view_start = new_view_start;
            return;
        }

        // try moving slider backward
        while new_view_start > new_position && new_view_start > 0 {
            if new_view_start < height {
                new_view_start = 0;
                break;
            }
            new_view_start -= height;
        }
        self.view_start = new_view_start;
    }

    /**
     * Count all visible elements
     */
    fn count_elements(&self) -> usize {
        let mut count = 0;
        for item in &self.items {
            if !item.expanded {
                continue;
            }
            count += 1;
        }
        count
    }

    fn toggle_item(&mut self) -> Option<PathBuf> {
        let item_option = self.items.get_mut(self.selected_index);
        let selected_path: Option<PathBuf>;
        match item_option {
            Some(mut item) => {
                if !item.is_dir {
                    selected_path = Some(item.path.clone());
                } else {
                    item.expanded = !item.expanded;
                    selected_path = None;
                }
            }
            None => {
                log::warn!("Failed finding item at position: {:?}", self.selected_index);
                selected_path = None;
            }
        }
        selected_path
    }

    fn select_item(&mut self) -> Option<PathBuf> {
        let selected_path = self.toggle_item();

        if selected_path.is_none() {
            let item = self.items[self.selected_index].clone();

            let mut item_path = String::from(item.path.as_path().to_str().unwrap_or(""));
            // appending platform file separator solves the issue with expanding same-prefixed
            // folders (ie. .git, .github)
            item_path.push(std::path::MAIN_SEPARATOR);
            for current_item in self.items.iter_mut() {
                if current_item.path == item.path {
                    continue;
                }
                if current_item.depth != item.depth + 1 {
                    // when opening dirs only toggle 1 level lower
                    continue;
                }
                if String::from(current_item.path.as_path().to_str().unwrap_or(""))
                    .starts_with(&item_path)
                {
                    current_item.visible = item.expanded;
                }
            }
        }

        selected_path
    }

    fn move_up(&mut self, window_buffer: Buffer) -> Event {
        let mut new_selected_index = if self.selected_index > 0 {
            self.selected_index - 1
        } else {
            self.selected_index
        };

        while new_selected_index > 0 && !self.items[new_selected_index].visible {
            new_selected_index -= 1;
        }

        self.align_vertical(new_selected_index, &window_buffer);
        self.selected_index = new_selected_index;
        Event::new()
    }

    fn move_down(&mut self, window_buffer: Buffer) -> Event {
        let mut current_index = self.selected_index + 1;
        while current_index < (self.items.len() - 1) && !self.items[current_index].visible {
            current_index += 1;
        }

        current_index = cmp::min(self.items.len() - 1, current_index);

        self.align_vertical(current_index, &window_buffer);
        self.selected_index = current_index;
        Event::new()
    }
}

#[derive(Clone, Debug)]
struct FileItem {
    expanded: bool,
    visible: bool,
    is_dir: bool,
    depth: usize,
    // from path it should be easy to decypher parent path
    path: PathBuf,
}

impl FileItem {
    fn new_buffer(_uuid: Uuid) -> Self {
        Self {
            visible: true,
            expanded: false,
            is_dir: false,
            depth: 0,
            path: PathBuf::new(),
        }
    }
}

impl HandleKey for OpenFileMenu {
    fn handle_key(&mut self, ekey: ExtendedKey, window_buffer: Buffer) -> Event {
        let keep_open = Event::new();
        match ekey.key {
            Key::Up | Key::Char(GAME_UP_SHORTCUT) => {
                self.move_up(window_buffer);
            }
            Key::PageUp => {
                for _ in 0..self.height(&window_buffer) {
                    self.move_up(window_buffer);
                }
                return keep_open;
            }
            Key::Down => {
                self.move_down(window_buffer);
            }
            Key::PageDown => {
                for _ in 0..self.height(&window_buffer) {
                    self.move_down(window_buffer);
                }
                return keep_open;
            }
            Key::Left => {
                // TODO:
            }
            Key::Right => {
                // TODO:
            }
            HELP_SHORTCUT => {
                return Event {
                    window_event: Some(WindowEvent::open(ControlType::HelpOverlay)),
                    ..Event::new()
                };
            }
            Key::Enter | Key::Char(SPACE) => match self.mode {
                OpenMenuMode::File => {
                    let selected_path = self.select_item();
                    let some_path_selected = selected_path != None;
                    let open_file_event = if some_path_selected {
                        Some(OpenFileEvent {
                            path: selected_path.unwrap(),
                        })
                    } else {
                        None
                    };
                    return Event {
                        open_file_event,
                        window_event: if some_path_selected {
                            Some(WindowEvent::close(ControlType::OpenFileMenu, None))
                        } else {
                            None
                        },
                        ..Event::new()
                    };
                }
                OpenMenuMode::Buffer => {
                    let selected_path = &self.items[self.selected_index];
                    return Event {
                        open_file_event: Some(OpenFileEvent {
                            path: selected_path.path.clone(),
                        }),
                        window_event: Some(WindowEvent::close(ControlType::OpenFileMenu, None)),
                        ..Event::new()
                    };
                }
            },
            Key::Char(c) => {
                if ekey.modifiers.ctrl {
                    match c {
                        CTRL_SEARCH_SHORTCUT => {
                            return Event {
                                window_event: Some(WindowEvent::open(ControlType::SearchOverlay)),
                                ..Event::key(ekey)
                            }
                        }
                        CTRL_MENU_SHORTCUT => {
                            return Event {
                                window_event: Some(WindowEvent::close(
                                    ControlType::OpenFileMenu,
                                    None,
                                )),
                                ..Event::key(ekey)
                            }
                        }
                        _ => {}
                    }
                } else if let GAME_DOWN_SHORTCUT = c {
                    self.move_down(window_buffer);
                }
            }
            Key::Esc => {
                return Event {
                    window_event: Some(WindowEvent::close(ControlType::OpenFileMenu, None)),
                    ..Event::key(ekey)
                };
            }
            _ => {}
        }
        Event::new()
    }
}

impl Render for OpenFileMenu {
    fn render(&self, buffer: &Buffer) {
        self.render_window(buffer);

        let max_displayed = buffer.size.rows - 2;

        let mut items_to_render: Vec<FileItem> = Vec::new();
        let mut min_components = std::usize::MAX;

        let mut count = self.view_start;
        let mut selected_item = self.selected_index;
        let mut count_invisible = 0;

        let mut visibility_stack: Vec<bool> = Vec::new();
        while items_to_render.len() < max_displayed && count < self.items.len() {
            let item = &self.items[count];

            while item.depth < visibility_stack.len() {
                visibility_stack.pop();
            }

            let vis_stack_len = visibility_stack.len();
            if vis_stack_len == 0 || item.depth > vis_stack_len {
                visibility_stack.push(item.visible)
            } else if item.depth == vis_stack_len {
                visibility_stack[vis_stack_len - 1] = item.visible
            }

            // if at least one parent is hidden hide all subitems
            let mut skip = false;
            for visible in &visibility_stack {
                if !visible {
                    skip = true;
                    break;
                }
            }
            if skip {
                count += 1;
                count_invisible += 1;
                continue;
            }

            min_components = cmp::min(min_components, item.path.components().count());
            if count == self.selected_index {
                selected_item = count - count_invisible - self.view_start;
            }
            items_to_render.push(item.clone());
            count += 1;
        }

        let top_left: Location = Location::new(
            self.get_origin().row + buffer.editor_top_left.row,
            self.get_origin().column + buffer.editor_top_left.column,
        );

        let mode = if !self.pattern.is_empty() {
            OpenMenuMode::Buffer
        } else {
            self.mode
        };
        for (item_index, item) in items_to_render.iter().enumerate() {
            let padding_top_left = [1, 1];
            let row_spacing = 0;

            let row = top_left.row + WINDOW_BORDER[0] + item_index + row_spacing * (item_index + 1);
            let column = top_left.column + WINDOW_BORDER[1] + padding_top_left[1];
            let location = Location::new(row, column);

            let comp_prefix = item.path.components().count() - min_components;
            let mut dir_prefix = String::from("");
            if mode == OpenMenuMode::File {
                for _ in 0..comp_prefix {
                    dir_prefix += "  ";
                }
            }

            let prefix = if item.is_dir && mode == OpenMenuMode::File {
                "/"
            } else {
                ""
            };
            let path_name = if mode == OpenMenuMode::File {
                item.path
                    .as_path()
                    .file_name()
                    .unwrap_or_else(|| std::ffi::OsStr::new(""))
                    .to_str()
            } else {
                item.path.as_path().to_str()
            };

            let name = format!("{}{}{}", dir_prefix, prefix, path_name.unwrap_or(""));
            let style = if item_index == selected_item {
                INVERSE_STYLE
            } else {
                NORMAL_STYLE
            };
            buffer.write_direct(&name, location, style);
        }
    }
}

impl Window for OpenFileMenu {
    fn get_origin(&self) -> Location {
        Location::new(0, 0)
    }

    fn get_size(&self) -> Size {
        self.size
    }

    fn get_title(&self) -> String {
        self.title.clone()
    }
}

impl HandleSearchEvent for OpenFileMenu {
    fn handle_search_event(&mut self, search_event: SearchEvent, _window_buffer: Buffer) -> Event {
        self.pattern = search_event.pattern;

        match self.mode {
            OpenMenuMode::File => {
                if let Some(last_loaded) = self.last_loaded_dir.clone() {
                    let loaded_dir = self.load_directory(last_loaded, LOAD_DIRECTORY_DEPTH);
                    if let Ok(dir) = loaded_dir {
                        self.selected_index = 0;
                        self.items = dir;
                    } else {
                        log::warn!("Failed reloading dir: {:?}", self.last_loaded_dir);
                    }
                }
            }
            OpenMenuMode::Buffer => {
                self.reload_buffers();
            }
        }
        Event::new()
    }
}

/**
 * Top-level editor state
 */
pub struct Editor {
    window_buffer: Buffer,

    pub file_buffer_controls: HashMap<Uuid, FileBuffer>,
    menu_controls: HashMap<Uuid, Menu>,
    open_file_controls: HashMap<Uuid, OpenFileMenu>,
    input_dialog_controls: HashMap<Uuid, YesNoDialog>,
    save_as_dialog_controls: HashMap<Uuid, InputDialog>,
    undo_redo_overlays: HashMap<Uuid, UndoRedoOverlay>,
    selection_overlay_buffers: HashMap<Uuid, SelectionOverlay>,
    search_overlays: HashMap<Uuid, SearchOverlay>,
    help_overlays: HashMap<Uuid, HelpOverlay>,

    // currently displayed file buffer
    active_file_buffer: Option<Uuid>,
    // inserted window with highest priority gets displayed
    controls: Vec<ControlReference>,

    // syntax highlighing
    syntax_highlight: SyntaxHighlight,
}

impl Editor {
    pub fn new() -> Self {
        Self {
            window_buffer: Buffer::new(20, 20),
            file_buffer_controls: HashMap::new(),
            menu_controls: HashMap::new(),
            open_file_controls: HashMap::new(),
            input_dialog_controls: HashMap::new(),
            save_as_dialog_controls: HashMap::new(),
            undo_redo_overlays: HashMap::new(),
            selection_overlay_buffers: HashMap::new(),
            search_overlays: HashMap::new(),
            help_overlays: HashMap::new(),
            active_file_buffer: None,
            controls: Vec::new(),
            syntax_highlight: SyntaxHighlight::new(),
        }
    }

    pub fn init(&mut self, output_mode: rustbox::OutputMode) -> Result<String, Box<dyn Error>> {
        unsafe {
            let init_options = InitOptions {
                output_mode,
                ..Default::default()
            };

            match RustBox::init(init_options) {
                Result::Ok(v) => {
                    EDITOR_BUFFER = Some(v);
                }
                Result::Err(e) => {
                    log::warn!("Failed initializing rust box: {}", e);
                    panic!("Failed initializing rustbox");
                }
            };
        }

        self.window_buffer.hide_cursor();

        let args: Vec<String> = env::args().collect();
        let size = terminal_size();
        if let Some((Width(w), Height(h))) = size {
            self.window_buffer.size = Size {
                rows: h as usize,
                columns: w as usize,
            };
            self.window_buffer.editor_size = self.window_buffer.size;
        } else {
            log::info!("Unable to get terminal size");
        }

        if args.len() > 1 {
            // Create a path to the desired file
            // TODO: open multiple files
            let file_path = Path::new(&args[1]);

            let pwd_path = env::current_dir()?;
            let joined_pathbuf: std::path::PathBuf = if file_path.is_relative() {
                let old_relative_path: &Path = file_path;
                pwd_path.as_path().join(old_relative_path)
            } else {
                file_path.to_path_buf()
            };

            if self.open_file_buffer(joined_pathbuf).is_err() {
                log::info!("Opened file: {:?}", file_path.display());
            } else {
                // show nice message to user that file path does not exist
            }
        }

        // show help overlay only when no files opened
        self.open_help_overlay(true);

        if self.file_buffer_controls.is_empty() && self.create_empty_file_buffer().is_err() {
            panic!("Failed creating an empty buffer.");
        }

        Ok("".to_string())
    }

    fn find_renderable_control(&self, control_reference: ControlReference) -> Option<&dyn Render> {
        // get control by uuid
        let uuid = control_reference.uuid;
        match control_reference.control_type {
            ControlType::FileBuffer => get_renderable(&self.file_buffer_controls, uuid),
            ControlType::Menu => get_renderable(&self.menu_controls, uuid),
            ControlType::YesNoDialog => get_renderable(&self.input_dialog_controls, uuid),
            ControlType::InputDialog => get_renderable(&self.save_as_dialog_controls, uuid),
            ControlType::UndoRedoOverlay => get_renderable(&self.undo_redo_overlays, uuid),
            ControlType::SelectionOverlay => get_renderable(&self.selection_overlay_buffers, uuid),
            ControlType::SearchOverlay => get_renderable(&self.search_overlays, uuid),
            ControlType::OpenFileMenu => get_renderable(&self.open_file_controls, uuid),
            ControlType::HelpOverlay => get_renderable(&self.help_overlays, uuid),
        }
    }

    fn is_displayed_on_top(&self, control_type: ControlType) -> bool {
        let control_reference_option = self.controls.last();
        match control_reference_option {
            Some(control_reference) => control_reference.control_type == control_type,
            None => false,
        }
    }

    #[allow(dead_code)]
    pub fn get_active_file_buffer(&self) -> Option<&FileBuffer> {
        if self.active_file_buffer == None {
            return None;
        }

        self.file_buffer_controls
            .get(&self.active_file_buffer.unwrap())
    }

    #[allow(dead_code)]
    pub fn get_active_file_buffer_mut(&mut self) -> Option<&mut FileBuffer> {
        if self.active_file_buffer == None {
            return None;
        }

        self.file_buffer_controls
            .get_mut(&self.active_file_buffer.unwrap())
    }

    fn open_save_dialog(&mut self) {
        if self.is_displayed_on_top(ControlType::YesNoDialog) {
            return;
        }

        let mut menu_items: LinkedList<MenuItem> = LinkedList::new();
        let active_file_buffer_option = self
            .file_buffer_controls
            .get_mut(&self.active_file_buffer.unwrap());
        match active_file_buffer_option {
            Some(active_file_buffer) => {
                let mut save_menu_item =
                    MenuItem::new(String::from("Save"), None, true, save_file_buffer_callback);
                save_menu_item.file_path = active_file_buffer.file_path.clone();
                // this duplicates the entire file which seems wastefull but on the other hand
                // Rust is a pass-by-value so why not
                save_menu_item.contents = active_file_buffer.contents.to_vec();

                menu_items.push_back(save_menu_item);
                menu_items.push_back(MenuItem::new(
                    String::from("Cancel"),
                    None,
                    false,
                    save_close_callback,
                ));

                let menu_size = Size::new(8, 60);
                let center = Location {
                    row: self.window_buffer.size.rows / 2,
                    column: self.window_buffer.size.columns / 2,
                };
                let top_left = Location {
                    row: center.row - menu_size.rows / 2,
                    column: center.column - menu_size.columns / 2,
                };

                let save_dialog = YesNoDialog {
                    text: format!("Save file? {}", active_file_buffer.file_path),
                    menu_items,
                    selected_index: 0,
                    title: String::from("Save Dialog"),
                    origin: top_left,
                    size: menu_size,
                };
                // when user is on editor and presses escape show top level menu
                let new_control_reference = ControlReference {
                    uuid: Uuid::new_v4(),
                    priority: 500,
                    control_type: ControlType::YesNoDialog,
                };
                self.input_dialog_controls
                    .insert(new_control_reference.uuid, save_dialog);
                self.controls.push(new_control_reference);
                self.controls
                    .sort_unstable_by(|c1, c2| c1.priority.partial_cmp(&c2.priority).unwrap());
            }
            None => {
                log::error!("Can't save empty file buffer");
            }
        }
    }

    fn close_save_dialog(&mut self, uuid: Uuid) {
        self.input_dialog_controls.remove(&uuid);
        self.controls
            .retain(|c| c.control_type != ControlType::YesNoDialog);
    }

    fn open_goto_dialog(&mut self) {
        if self.is_displayed_on_top(ControlType::InputDialog) {
            return;
        }

        let mut menu_items: LinkedList<MenuItem> = LinkedList::new();
        let active_file_buffer_option = self
            .file_buffer_controls
            .get_mut(&self.active_file_buffer.unwrap());
        match active_file_buffer_option {
            Some(active_file_buffer) => {
                let mut callback_item =
                    MenuItem::new(String::from("Goto Line"), None, true, goto_line_callback);
                callback_item.file_path = active_file_buffer.file_path.clone();
                // this duplicates the entire file which seems wastefull but on the other hand
                // Rust is a pass-by-value so why not
                callback_item.contents = active_file_buffer.contents.to_vec();

                menu_items.push_back(callback_item);
                menu_items.push_back(MenuItem::new(
                    String::from("Cancel"),
                    None,
                    false,
                    save_as_close_callback,
                ));

                let menu_size = Size::new(8, 60);
                let center = Location {
                    row: self.window_buffer.size.rows / 2,
                    column: self.window_buffer.size.columns / 2,
                };
                let top_left = Location {
                    row: center.row.saturating_sub(menu_size.rows / 2),
                    column: center.column.saturating_sub(menu_size.columns / 2),
                };

                let validator_regex = Regex::new(r"^[0-9]*$");
                if validator_regex.is_err() {
                    panic!("Failed compiling regex: {:?}", validator_regex);
                }

                let goto_dialog = InputDialog {
                    text: String::from("Goto line:"),
                    input: "".to_string(),
                    input_validation_regex: validator_regex.unwrap(),
                    input_location: 0,
                    menu_items,
                    selected_index: 0,
                    dialog_type: DialogType::Goto,
                    title: String::from("Goto line"),
                    origin: top_left,
                    size: menu_size,
                };
                // when user is on editor and presses escape show top level menu
                let new_control_reference = ControlReference {
                    uuid: Uuid::new_v4(),
                    priority: 500,
                    control_type: ControlType::InputDialog,
                };
                self.save_as_dialog_controls
                    .insert(new_control_reference.uuid, goto_dialog);
                self.controls.push(new_control_reference);
                self.controls
                    .sort_unstable_by(|c1, c2| c1.priority.partial_cmp(&c2.priority).unwrap());
            }
            None => {
                log::error!("Can't open goto dialog");
            }
        }
    }

    fn open_save_as_dialog(&mut self) {
        if self.is_displayed_on_top(ControlType::InputDialog) {
            return;
        }

        let mut menu_items: LinkedList<MenuItem> = LinkedList::new();
        let active_file_buffer_option = self
            .file_buffer_controls
            .get_mut(&self.active_file_buffer.unwrap());
        match active_file_buffer_option {
            Some(active_file_buffer) => {
                let mut save_menu_item = MenuItem::new(
                    String::from("Save As"),
                    None,
                    true,
                    save_file_buffer_callback,
                );
                save_menu_item.file_path = active_file_buffer.file_path.clone();
                // this duplicates the entire file which seems wastefull but on the other hand
                // Rust is a pass-by-value so why not
                save_menu_item.contents = active_file_buffer.contents.to_vec();

                menu_items.push_back(save_menu_item);
                menu_items.push_back(MenuItem::new(
                    String::from("Cancel"),
                    None,
                    false,
                    save_as_close_callback,
                ));

                let menu_size = Size::new(8, 60);
                let center = Location {
                    row: self.window_buffer.size.rows / 2,
                    column: self.window_buffer.size.columns / 2,
                };
                let top_left = Location {
                    row: center.row - menu_size.rows / 2,
                    column: center.column - menu_size.columns / 2,
                };

                let validator_regex = Regex::new(r".*");
                if validator_regex.is_err() {
                    panic!("Failed compiling regex: {:?}", validator_regex);
                }

                let save_dialog = InputDialog {
                    text: String::from("Save file As?"),
                    input: active_file_buffer.file_path.clone(),
                    input_validation_regex: validator_regex.unwrap(),
                    input_location: active_file_buffer.file_path.len(),
                    menu_items,
                    selected_index: 0,
                    dialog_type: DialogType::Save,
                    title: String::from("Save Dialog"),
                    origin: top_left,
                    size: menu_size,
                };
                // when user is on editor and presses escape show top level menu
                let new_control_reference = ControlReference {
                    uuid: Uuid::new_v4(),
                    priority: 500,
                    control_type: ControlType::InputDialog,
                };
                self.save_as_dialog_controls
                    .insert(new_control_reference.uuid, save_dialog);
                self.controls.push(new_control_reference);
                self.controls
                    .sort_unstable_by(|c1, c2| c1.priority.partial_cmp(&c2.priority).unwrap());
            }
            None => {
                log::error!("Can't save empty file buffer");
            }
        }
    }

    fn close_save_as_dialog(&mut self, uuid: Uuid) {
        self.save_as_dialog_controls.remove(&uuid);
        self.controls
            .retain(|c| c.control_type != ControlType::InputDialog);
    }

    fn open_undo_redo(&mut self) {
        if self.is_displayed_on_top(ControlType::UndoRedoOverlay) {
            return;
        }

        // TODO: set selection state on filebuffer
        let overlay = UndoRedoOverlay {
            title: String::from("Undo/Redo"),
            size: Size::new(3, self.window_buffer.size.columns),
            action: UndoRedoAction::Undo,
        };

        self.window_buffer.editor_top_left = Location::new(overlay.get_size().rows, 0);
        // resize editor buffer size
        self.window_buffer.editor_size = Size::new(
            self.window_buffer.size.rows - overlay.get_size().rows,
            self.window_buffer.size.columns,
        );

        // when user is on editor and presses escape show top level menu
        let new_control_reference = ControlReference::new(ControlType::UndoRedoOverlay, 500);
        self.undo_redo_overlays
            .insert(new_control_reference.uuid, overlay);
        self.controls.push(new_control_reference);
        self.controls
            .sort_unstable_by(|c1, c2| c1.priority.partial_cmp(&c2.priority).unwrap());
    }

    fn close_undo_redo_overlay(&mut self, uuid: Uuid) {
        // resize editor buffer size
        self.window_buffer.editor_top_left = Location::new(0, 0);
        self.window_buffer.editor_size = self.window_buffer.size;

        // TODO: make alignment nicer
        if let Some(file_buffer) = self
            .file_buffer_controls
            .get_mut(&self.active_file_buffer.unwrap())
        {
            file_buffer.coverage = None;
            file_buffer.coverage_start = None;
            file_buffer.coverage_end = None;
        }

        // update editor buffer top left and size
        self.undo_redo_overlays.remove(&uuid);
        self.controls
            .retain(|c| c.control_type != ControlType::UndoRedoOverlay);
    }

    fn open_select_overlay(&mut self) {
        if self.is_displayed_on_top(ControlType::SelectionOverlay) {
            return;
        }

        // TODO: set selection state on filebuffer
        let overlay = SelectionOverlay {
            title: String::from("Selection"),
            coverage: Coverage::FromTo,
            size: Size::new(3, self.window_buffer.size.columns),
        };

        self.window_buffer.editor_top_left = Location::new(overlay.get_size().rows, 0);
        // resize editor buffer size
        self.window_buffer.editor_size = Size::new(
            self.window_buffer.size.rows - overlay.get_size().rows,
            self.window_buffer.size.columns,
        );

        // when user is on editor and presses escape show top level menu
        let new_control_reference = ControlReference::new(ControlType::SelectionOverlay, 500);
        self.selection_overlay_buffers
            .insert(new_control_reference.uuid, overlay);
        self.controls.push(new_control_reference);
        self.controls
            .sort_unstable_by(|c1, c2| c1.priority.partial_cmp(&c2.priority).unwrap());

        // TODO: make alignment nicer
        if let Some(file_buffer) = self
            .file_buffer_controls
            .get_mut(&self.active_file_buffer.unwrap())
        {
            file_buffer.coverage = Some(Coverage::FromTo);
            file_buffer.align_buffer_vertical_up(&self.window_buffer);
            file_buffer.align_buffer_vertical_down(&self.window_buffer, false);
        }
    }

    fn close_select_overlay(&mut self, uuid: Uuid) {
        // resize editor buffer size
        self.window_buffer.editor_top_left = Location::new(0, 0);
        self.window_buffer.editor_size = self.window_buffer.size;

        // TODO: make alignment nicer
        if let Some(file_buffer) = self
            .file_buffer_controls
            .get_mut(&self.active_file_buffer.unwrap())
        {
            file_buffer.coverage = None;
            file_buffer.coverage_start = None;
            file_buffer.coverage_end = None;
        }

        // update editor buffer top left and size
        self.selection_overlay_buffers.remove(&uuid);
        self.controls
            .retain(|c| c.control_type != ControlType::SelectionOverlay);
    }

    fn open_search_overlay(&mut self, filter_mode: bool) {
        if self.is_displayed_on_top(ControlType::SearchOverlay) {
            return;
        }

        // TODO: set selection state on filebuffer
        let overlay = SearchOverlay {
            title: if filter_mode {
                String::from("Filter")
            } else {
                String::from("Search")
            },
            size: Size::new(3, self.window_buffer.size.columns),
            pattern_read_only: false,
            pattern_location: 0,
            pattern: String::from(""),
            filter_mode,
        };

        self.window_buffer.editor_top_left = Location::new(overlay.get_size().rows, 0);
        // resize editor buffer size
        self.window_buffer.editor_size = Size::new(
            self.window_buffer.size.rows - overlay.get_size().rows,
            self.window_buffer.size.columns,
        );

        let mut priority = 500;
        if !self.controls.is_empty() {
            priority = self.controls.last().unwrap().priority + 100;
        }

        // when user is on editor and presses escape show top level menu
        let new_control_reference = ControlReference::new(ControlType::SearchOverlay, priority);
        self.search_overlays
            .insert(new_control_reference.uuid, overlay);
        self.controls.push(new_control_reference);
        self.controls
            .sort_unstable_by(|c1, c2| c1.priority.partial_cmp(&c2.priority).unwrap());
    }

    fn close_search_overlay(&mut self, uuid: Uuid) {
        // resize editor buffer size
        self.window_buffer.editor_top_left = Location::new(0, 0);
        self.window_buffer.editor_size = self.window_buffer.size;

        // update editor buffer top left and size
        self.search_overlays.remove(&uuid);
        self.controls
            .retain(|c| c.control_type != ControlType::SearchOverlay);
    }

    fn open_help_overlay(&mut self, show_intro: bool) {
        if self.is_displayed_on_top(ControlType::HelpOverlay) {
            return;
        }

        let width = (self.window_buffer.size.columns / 3) * 2;
        let height = self.window_buffer.size.rows / 2;

        // TODO: set selection state on filebuffer
        let mut overlay = HelpOverlay {
            title: String::from("Help"),
            location: Location::new(
                (self.window_buffer.size.rows - height) / 2,
                (self.window_buffer.size.columns - width) / 2,
            ),
            size: Size::new(height, width),
            file_buffer: FileBuffer {
                read_only: true,
                ..FileBuffer::new(
                    Uuid::new_v4(),
                    self.syntax_highlight.find_syntax(PathBuf::new()),
                )
            },
        };

        if let Some(control_reference) = self.controls.last() {
            let file_contents = if show_intro {
                include_str!("help/intro.md")
            } else {
                match control_reference.control_type {
                    ControlType::FileBuffer => include_str!("help/buffer.md"),
                    ControlType::Menu => include_str!("help/menu.md"),
                    ControlType::OpenFileMenu => include_str!("help/openfile.md"),
                    ControlType::UndoRedoOverlay => include_str!("help/undoredo.md"),
                    ControlType::SelectionOverlay => include_str!("help/selection.md"),
                    ControlType::SearchOverlay => include_str!("help/search.md"),
                    _ => "",
                }
            };

            if file_contents.is_empty() {
                return;
            }

            for line in file_contents.split(NEWLINE) {
                let mut char_vec: Vec<char> = line.chars().collect();
                char_vec.push(NEWLINE);
                overlay.file_buffer.lines.push_line(char_vec.len());
                overlay.file_buffer.contents.append(&mut char_vec);
            }
        } else {
            let file_contents = include_str!("help/intro.md");
            for line in file_contents.split(NEWLINE) {
                let mut char_vec: Vec<char> = line.chars().collect();
                char_vec.push(NEWLINE);
                overlay.file_buffer.lines.push_line(char_vec.len());
                overlay.file_buffer.contents.append(&mut char_vec);
            }
        }

        let mut priority = 500;
        if !self.controls.is_empty() {
            priority = self.controls.last().unwrap().priority + 100;
        }

        // when user is on editor and presses escape show top level menu
        let new_control_reference = ControlReference::new(ControlType::HelpOverlay, priority);
        self.help_overlays
            .insert(new_control_reference.uuid, overlay);
        self.controls.push(new_control_reference);
        self.controls
            .sort_unstable_by(|c1, c2| c1.priority.partial_cmp(&c2.priority).unwrap());
    }

    fn close_help_overlay(&mut self, uuid: Uuid) {
        // resize editor buffer size
        self.window_buffer.editor_top_left = Location::new(0, 0);
        self.window_buffer.editor_size = self.window_buffer.size;

        // update editor buffer top left and size
        self.help_overlays.remove(&uuid);
        self.controls
            .retain(|c| c.control_type != ControlType::HelpOverlay);
    }

    fn open_main_menu(&mut self) {
        if self.is_displayed_on_top(ControlType::Menu) {
            return;
        }

        let mut menu_items: LinkedList<MenuItem> = LinkedList::new();
        menu_items.push_back(MenuItem::new(
            String::from("New"),
            Some('e'),
            false,
            new_callback,
        ));
        menu_items.push_back(MenuItem::new(
            String::from("Open File"),
            Some('d'),
            true,
            open_callback,
        ));
        menu_items.push_back(MenuItem::new(
            String::from("Open Buffer"),
            Some('f'),
            true,
            open_buffer_callback,
        ));
        menu_items.push_back(MenuItem::new(
            String::from("Save"),
            Some('s'),
            true,
            save_callback,
        ));
        menu_items.push_back(MenuItem::new(
            String::from("Save As"),
            Some('a'),
            false,
            save_as_callback,
        ));
        menu_items.push_back(MenuItem::new(
            String::from("Exit"),
            Some('x'),
            false,
            exit_callback,
        ));

        let menu_size = Size::new(20, 40);
        let center = Location {
            row: self.window_buffer.size.rows / 2,
            column: self.window_buffer.size.columns / 2,
        };
        let top_left = Location {
            row: center.row.saturating_sub(menu_size.rows / 2),
            column: center.column.saturating_sub(menu_size.columns / 2),
        };

        let top_level_menu = Menu {
            items: menu_items,
            selected_index: 0,
            title: String::from("Main Menu"),
            origin: top_left,
            size: menu_size,
        };

        // when user is on editor and presses escape show top level menu
        let new_control_reference = ControlReference {
            uuid: Uuid::new_v4(),
            priority: MENU_PRIORITY,
            control_type: ControlType::Menu,
        };
        self.menu_controls
            .insert(new_control_reference.uuid, top_level_menu);
        self.controls.push(new_control_reference);
        self.controls
            .sort_unstable_by(|c1, c2| c1.priority.partial_cmp(&c2.priority).unwrap());
    }

    fn close_main_menu(&mut self, uuid: Uuid) {
        self.menu_controls.remove(&uuid);
        self.controls
            .retain(|c| c.control_type != ControlType::Menu);
        self.controls
            .sort_unstable_by(|c1, c2| c1.priority.partial_cmp(&c2.priority).unwrap());
    }

    fn open_buffers_menu(&mut self) -> Result<(), std::io::Error> {
        if self.is_displayed_on_top(ControlType::OpenFileMenu) {
            // return;
            return Ok(());
        }

        let mut open_buffer_menu = OpenFileMenu {
            mode: OpenMenuMode::Buffer,
            view_start: 0,
            original_items: Vec::new(),
            items: Vec::new(),
            selected_index: 0,
            total_items: 0,
            title: String::from("Open Buffer"),
            size: self.window_buffer.size,
            last_loaded_dir: None,
            pattern: "".to_string(),
        };

        let mut file_buffers: Vec<FileBuffer> = Vec::new();
        for file_buffer in self.file_buffer_controls.values() {
            file_buffers.push((*file_buffer).clone());
        }
        if let Ok(items) = open_buffer_menu.load_buffers(&file_buffers) {
            for item in items {
                open_buffer_menu.items.push(item.clone());
                open_buffer_menu.original_items.push(item.clone());
            }
        }
        open_buffer_menu.total_items = open_buffer_menu.count_elements();

        // when user is on editor and presses escape show top level menu
        let new_control_reference = ControlReference {
            uuid: Uuid::new_v4(),
            priority: OPEN_FILE_PRIORITY,
            control_type: ControlType::OpenFileMenu,
        };
        self.open_file_controls
            .insert(new_control_reference.uuid, open_buffer_menu);
        self.controls.push(new_control_reference);
        self.controls
            .sort_unstable_by(|c1, c2| c1.priority.partial_cmp(&c2.priority).unwrap());
        Ok(())
    }

    fn open_file_menu(&mut self) -> Result<(), std::io::Error> {
        if self.is_displayed_on_top(ControlType::OpenFileMenu) {
            // return;
            return Ok(());
        }

        let mut open_file_menu = OpenFileMenu {
            mode: OpenMenuMode::File,
            view_start: 0,
            original_items: Vec::new(),
            items: Vec::new(),
            selected_index: 0,
            total_items: 0,
            title: String::from("Open File"),
            size: self.window_buffer.size,
            last_loaded_dir: None,
            pattern: "".to_string(),
        };

        let current_path = env::current_dir()?;
        open_file_menu.last_loaded_dir = Some(current_path.clone());
        if let Ok(item) = open_file_menu.load_directory(current_path, LOAD_DIRECTORY_DEPTH) {
            open_file_menu.items = item
        }
        open_file_menu.total_items = open_file_menu.count_elements();

        // when user is on editor and presses escape show top level menu
        let new_control_reference = ControlReference {
            uuid: Uuid::new_v4(),
            priority: OPEN_FILE_PRIORITY,
            control_type: ControlType::OpenFileMenu,
        };
        self.open_file_controls
            .insert(new_control_reference.uuid, open_file_menu);
        self.controls.push(new_control_reference);
        self.controls
            .sort_unstable_by(|c1, c2| c1.priority.partial_cmp(&c2.priority).unwrap());
        Ok(())
    }

    fn close_file_menu(&mut self, uuid: Uuid) {
        self.open_file_controls.remove(&uuid);
        self.controls
            .retain(|c| c.control_type != ControlType::OpenFileMenu);
        self.controls
            .sort_unstable_by(|c1, c2| c1.priority.partial_cmp(&c2.priority).unwrap());
    }

    fn create_empty_file_buffer(&mut self) -> Result<(), Box<dyn Error>> {
        // Open the path in read-only mode, returns `io::Result<File>`
        let pwd_path = env::current_dir()?;
        let mut file_buffer = FileBuffer {
            contents: vec!['\n'],
            file_path: pwd_path.to_str().unwrap_or("").to_string(),
            show_line_number: true,
            ..FileBuffer::new(
                Uuid::new_v4(),
                self.syntax_highlight.find_syntax(PathBuf::new()),
            )
        };

        file_buffer.lines.push_line(1);

        let new_control_reference = ControlReference {
            uuid: file_buffer.buffer_id,
            priority: VISIBLE_FILE_BUFFER_PRIORITY,
            control_type: ControlType::FileBuffer,
        };
        self.file_buffer_controls
            .insert(new_control_reference.uuid, file_buffer);
        self.controls.push(new_control_reference);
        self.controls
            .sort_unstable_by(|c1, c2| c1.priority.partial_cmp(&c2.priority).unwrap());

        // deprioritise currently selected buffer
        if self.active_file_buffer != None {
            let active_uuid = self.active_file_buffer.unwrap();
            if let Some(control) = self.controls.iter_mut().find(|x| x.uuid == active_uuid) {
                control.priority = DEFAULT_FILE_BUFFER_PRIORITY;
            }
        }
        self.active_file_buffer = Some(new_control_reference.uuid);
        Ok(())
    }

    fn open_file_buffer(&mut self, pathbuf: PathBuf) -> Result<(), Box<dyn Error>> {
        let file_path = pathbuf.as_path();
        if let Some((uuid, _file_buffer)) = self
            .file_buffer_controls
            .iter_mut()
            .find(|(_key, value)| value.file_path == file_path.to_str().unwrap_or(""))
        {
            for control in self.controls.iter_mut() {
                if control.uuid == *uuid {
                    control.priority = VISIBLE_FILE_BUFFER_PRIORITY;
                } else {
                    control.priority = DEFAULT_FILE_BUFFER_PRIORITY;
                }
            }
            self.active_file_buffer = Some(*uuid);
            self.controls
                .sort_unstable_by(|c1, c2| c1.priority.partial_cmp(&c2.priority).unwrap());
            return Ok(());
        }

        let metadata = fs::metadata(file_path)?;
        let estimated_size = metadata.len();

        // Open the path in read-only mode, returns `io::Result<File>`
        let file = match OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&file_path)
        {
            Err(why) => {
                log::info!("Couldn't create file: {:?}", why);
                panic!("Couldn't create file: {}", file_path.display());
            }
            Ok(file) => file,
        };

        let mut file_buffer = FileBuffer {
            contents: Vec::with_capacity(estimated_size as usize),
            show_line_number: true,
            ..FileBuffer::new(
                Uuid::new_v4(),
                self.syntax_highlight.find_syntax(pathbuf.clone()),
            )
        };
        let mut buf_reader = BufReader::with_capacity(1024 * 10, file);

        const BUFFER_SIZE: usize = 1024;
        let mut buffer: [u8; BUFFER_SIZE] = [0; BUFFER_SIZE];
        let mut line: Vec<char> = Vec::new();
        loop {
            let read_bytes = buf_reader.read(&mut buffer)?;
            for rawch in &buffer[0..read_bytes] {
                let ch = *rawch as char;
                line.push(ch);
                if ch == NEWLINE {
                    file_buffer.lines.push_line(line.len());
                    file_buffer.contents.append(&mut line);
                    line = Vec::new();
                }
            }
            if read_bytes != BUFFER_SIZE {
                break;
            }
        }
        if !line.is_empty() {
            // this can happen if last or only line in file doesn't have a NEWLINE char
            file_buffer.lines.push_line(line.len());
            file_buffer.contents.append(&mut line);
        }

        match file_path.to_str() {
            Some(path_str) => {
                file_buffer.file_path = path_str.to_string();
            }
            None => {
                log::error!("Failed loading file: invalid file system path");
            }
        }

        let new_control_reference = ControlReference {
            uuid: file_buffer.buffer_id,
            priority: VISIBLE_FILE_BUFFER_PRIORITY,
            control_type: ControlType::FileBuffer,
        };

        // deprioritise currently selected buffer
        if self.active_file_buffer != None {
            let active_uuid = self.active_file_buffer.unwrap();
            if let Some(control) = self.controls.iter_mut().find(|x| x.uuid == active_uuid) {
                control.priority = DEFAULT_FILE_BUFFER_PRIORITY;
            }
        }

        self.file_buffer_controls
            .insert(new_control_reference.uuid, file_buffer);
        self.controls.push(new_control_reference);
        self.controls
            .sort_unstable_by(|c1, c2| c1.priority.partial_cmp(&c2.priority).unwrap());

        self.active_file_buffer = Some(new_control_reference.uuid);
        Ok(())
    }

    fn handle_key(&mut self, ekey: ExtendedKey) -> Option<Event> {
        let control_reference_option = self.controls.last();
        match control_reference_option {
            Some(control_reference) => {
                // get control by uuid
                let uuid = control_reference.uuid;
                let buffer = self.window_buffer;
                match control_reference.control_type {
                    ControlType::FileBuffer => {
                        handle_key(&mut self.file_buffer_controls, uuid, ekey, buffer)
                    }
                    ControlType::Menu => handle_key(&mut self.menu_controls, uuid, ekey, buffer),
                    ControlType::OpenFileMenu => {
                        handle_key(&mut self.open_file_controls, uuid, ekey, buffer)
                    }
                    ControlType::YesNoDialog => {
                        handle_key(&mut self.input_dialog_controls, uuid, ekey, buffer)
                    }
                    ControlType::InputDialog => {
                        handle_key(&mut self.save_as_dialog_controls, uuid, ekey, buffer)
                    }
                    ControlType::UndoRedoOverlay => {
                        handle_key(&mut self.undo_redo_overlays, uuid, ekey, buffer)
                    }
                    ControlType::SelectionOverlay => {
                        handle_key(&mut self.selection_overlay_buffers, uuid, ekey, buffer)
                    }
                    ControlType::SearchOverlay => {
                        handle_key(&mut self.search_overlays, uuid, ekey, buffer)
                    }
                    ControlType::HelpOverlay => {
                        handle_key(&mut self.help_overlays, uuid, ekey, buffer)
                    }
                }
            }
            _ => {
                log::info!("No controls to be rendered");
                None
            }
        }
    }

    pub fn handle_event(&mut self, event: Event) -> bool {
        if event.bubble_down {
            match self
                .file_buffer_controls
                .get_mut(&self.active_file_buffer.unwrap())
            {
                Some(file_buffer) => {
                    file_buffer.handle_key(event.key.unwrap(), self.window_buffer);
                }
                None => {
                    log::info!("No active buffer - not bubbling events down.");
                }
            }
        }

        if event.create_file_event.is_some() && self.create_empty_file_buffer().is_err() {
            panic!("Failed creating an empty buffer.");
        }

        if event.select_event.is_some() {
            let select_event = event.select_event.unwrap();
            match self
                .file_buffer_controls
                .get_mut(&self.active_file_buffer.unwrap())
            {
                Some(file_buffer) => {
                    file_buffer.handle_select_event(select_event);
                }
                None => {
                    log::warn!("Searched in empty buffer?");
                }
            }
        }

        if event.undo_event.is_some() {
            let undo_event = event.undo_event.unwrap();
            match self
                .file_buffer_controls
                .get_mut(&self.active_file_buffer.unwrap())
            {
                Some(file_buffer) => {
                    file_buffer.handle_undo_event(undo_event, self.window_buffer);
                }
                None => {
                    log::warn!("Searched in empty buffer?");
                }
            }
        }

        if event.search_event.is_some() {
            if self.controls.len() > 2
                && self.controls[self.controls.len() - 2].control_type == ControlType::OpenFileMenu
            {
                let open_file_control = self.controls[self.controls.len() - 2];
                self.open_file_controls
                    .get_mut(&open_file_control.uuid)
                    .unwrap()
                    .handle_search_event(event.search_event.unwrap(), self.window_buffer);
            } else if let Some(file_buffer) = self
                .file_buffer_controls
                .get_mut(&self.active_file_buffer.unwrap())
            {
                // render first one, TODO: handle multiple buffers
                file_buffer.handle_search_event(event.search_event.unwrap(), self.window_buffer);
            }
        }

        let mut opened_file = false;
        if event.open_file_event.is_some() {
            let pathbuf = event.open_file_event.unwrap().path;
            if self.open_file_buffer(pathbuf).is_err() {
                // TODO: User facing messages
                log::warn!("Failed opening file.");
            }
            opened_file = true;
        }

        if let Some(window_event) = event.window_event {
            match window_event.action {
                WindowAction::Open => {
                    self.handle_open_window_event(window_event);
                }
                WindowAction::Close => {
                    self.handle_close_window_event(window_event.clone());
                    // TODO: add a nice way for dialogs to close main menu as well
                    if opened_file || window_event.control_type == ControlType::YesNoDialog {
                        self.handle_close_window_event(WindowEvent::close(ControlType::Menu, None));
                    }
                }
            }
        }

        if let Some(goto_event) = event.gotoline_event {
            if let Some(file_buffer) = self
                .file_buffer_controls
                .get_mut(&self.active_file_buffer.unwrap())
            {
                file_buffer.handle_goto_event(goto_event, self.window_buffer);
            }
        }

        event.exit_event.is_none()
    }

    pub fn main_loop(&mut self) {
        self.render();
        loop {
            let duration = Duration::from_millis(100);
            let event_option = self.window_buffer.peek_event(duration);
            if !self.handle_event_option(event_option) {
                break;
            }
        }

        // force rust_box to go out of scope and be destroyed
        // if not destroyed it causes weird aliasing in shell
        unsafe {
            EDITOR_BUFFER = None;
        }
    }

    pub fn handle_event_option(&mut self, event_option: Option<rustbox::EventResult>) -> bool {
        match event_option {
            Some(event) => {
                match event {
                    Ok(rustbox::Event::KeyEvent(key)) => {
                        match self.handle_key(key) {
                            Some(result) => {
                                if !self.handle_event(result) {
                                    return false;
                                }
                            }
                            _ => {
                                log::warn!("Failed processing key stroke.");
                                return true;
                            }
                        }
                        self.render();
                    }
                    Ok(rustbox::Event::NoEvent) => { /* noop */ }
                    Err(e) => {
                        log::warn!("Failed on editing command: {}", e);
                    }
                    _ => {
                        return false;
                    }
                }
            }
            _ => {
                panic!("Failed initializing RustBox.");
            }
        }
        true
    }

    fn render(&mut self) {
        self.window_buffer.clear();

        // render top control only
        // TODO: render underlying window for orientation
        let control_reference_option = self.controls.last();
        match control_reference_option {
            Some(control_reference) => {
                if control_reference.is_overlay() {
                    // check if underlying control is open file and render it instead
                    if self.controls.len() > 2
                        && ((self.controls[self.controls.len() - 2].control_type
                            == ControlType::OpenFileMenu)
                            || (control_reference.control_type == ControlType::HelpOverlay))
                    {
                        match self.find_renderable_control(self.controls[self.controls.len() - 2]) {
                            Some(renderable_control) => {
                                renderable_control.render(&self.window_buffer);
                            }
                            None => {
                                // dev error
                                log::error!(
                                    "Failed to find a control with uuid: {}",
                                    control_reference.uuid
                                );
                            }
                        }
                    } else if let Some(file_buffer) = self
                        .file_buffer_controls
                        .get_mut(&self.active_file_buffer.unwrap())
                    {
                        // render first one, TODO: handle multiple buffers
                        file_buffer.render(&self.window_buffer);
                    }
                }

                match self.find_renderable_control(*control_reference) {
                    Some(renderable_control) => {
                        renderable_control.render(&self.window_buffer);
                    }
                    None => {
                        // dev error
                        log::error!(
                            "Failed to find a control with uuid: {}",
                            control_reference.uuid
                        );
                    }
                }
            }
            None => {
                // simply close editor if no files opened
                // TODO: suggest creating a new file
                log::info!("No files opened. Closing...");
            }
        }

        self.window_buffer.present();
    }
}

impl HandleEvent for Editor {
    fn handle_event(&self, _search_event: Event) -> Event {
        Event::new()
    }
}

impl HandleWindowEvent for Editor {
    fn handle_open_window_event(&mut self, window_event: WindowEvent) -> Event {
        match window_event.control_type {
            ControlType::FileBuffer => {
                if self.open_buffers_menu().is_err() {
                    log::warn!("Failed opening up buffers menu.");
                }
            }
            ControlType::Menu => {
                self.open_main_menu();
            }
            ControlType::OpenFileMenu => {
                if self.open_file_menu().is_err() {
                    log::warn!("Failed opening up file menu.");
                }
            }
            ControlType::YesNoDialog => {
                self.open_save_dialog();
            }
            ControlType::InputDialog => {
                if let Some(dialog_type) = window_event.dialog_type {
                    match dialog_type {
                        DialogType::Save => {
                            self.open_save_as_dialog();
                        }
                        DialogType::Goto => {
                            self.open_goto_dialog();
                        }
                    }
                } else {
                    panic!("Unknown dialog type");
                }
            }
            ControlType::UndoRedoOverlay => {
                self.open_undo_redo();
            }
            ControlType::SelectionOverlay => {
                self.open_select_overlay();
            }
            ControlType::HelpOverlay => {
                self.open_help_overlay(false);
            }
            ControlType::SearchOverlay => {
                let filter_mode = self.is_displayed_on_top(ControlType::OpenFileMenu);
                self.open_search_overlay(filter_mode);
            }
        }
        Event::new()
    }

    fn handle_close_window_event(&mut self, window_event: WindowEvent) -> Event {
        let mut control_id_option: Option<Uuid> = None;
        if window_event.control_id == None {
            for control in &self.controls[..] {
                if control.control_type == window_event.control_type {
                    control_id_option = Some(control.uuid);
                }
            }
        } else {
            control_id_option = window_event.control_id;
        }
        if control_id_option == None {
            log::warn!("Failed closing unknown control: {:?}", window_event);
            return Event::new();
        }
        let control_id: Uuid = control_id_option.unwrap();
        match window_event.control_type {
            ControlType::FileBuffer => {
                // TODO
            }
            ControlType::Menu => {
                self.close_main_menu(control_id);
            }
            ControlType::OpenFileMenu => {
                self.close_file_menu(control_id);
            }
            ControlType::YesNoDialog => {
                self.close_save_dialog(control_id);
            }
            ControlType::InputDialog => {
                self.close_save_as_dialog(control_id);
            }
            ControlType::UndoRedoOverlay => {
                self.close_undo_redo_overlay(control_id);
            }
            ControlType::SelectionOverlay => {
                self.close_select_overlay(control_id);
            }
            ControlType::SearchOverlay => {
                self.close_search_overlay(control_id);
            }
            ControlType::HelpOverlay => {
                self.close_help_overlay(control_id);
            }
        }
        Event::new()
    }
}

fn is_render_location_in_buffer(text_location: Location, top_left: Location, size: Size) -> bool {
    text_location.row >= (top_left.row)
        && text_location.column >= (top_left.column)
        && text_location.row < (top_left.row + size.rows)
        && text_location.column < (top_left.column + size.columns)
}

fn is_location_in_buffer(
    text_location: Location,
    view_location: Location,
    top_left: Location,
    size: Size,
) -> bool {
    text_location.row >= (top_left.row + view_location.row)
        && text_location.column >= (top_left.column + view_location.column)
        && text_location.row < (top_left.row + view_location.row + size.rows)
        && text_location.column < (top_left.column + view_location.column + size.columns)
}
