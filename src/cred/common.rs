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
extern crate rustbox;

use rustbox::Color;
use std::slice::Iter;

/**
 * This is part of a workaround to make UI controls "live" in a single heap
 *
 * Each element of that heap has a reference to a control.
 * Each control is stored in a separate field
 */
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ControlType {
    FileBuffer,
    Menu,
    OpenFileMenu,
    YesNoDialog,
    InputDialog,
    UndoRedoOverlay,
    SelectionOverlay,
    SearchOverlay,
    HelpOverlay,
}

#[derive(Clone, Debug)]
pub struct FontStyle {
    pub font_style: rustbox::Style,
    pub foreground_color: rustbox::Color,
    pub background_color: rustbox::Color,
}

impl FontStyle {
    /**
     * If text pointer is on also selected portion of text
     */
    pub fn select_pointer(&self) -> FontStyle {
        FontStyle {
            font_style: self.font_style,
            foreground_color: self.background_color,
            background_color: Color::Blue,
        }
    }

    pub fn invert(&self) -> FontStyle {
        FontStyle {
            font_style: self.font_style,
            foreground_color: self.background_color,
            background_color: self.foreground_color,
        }
    }
}

pub const NORMAL_STYLE: FontStyle = FontStyle {
    font_style: rustbox::RB_NORMAL,
    foreground_color: Color::White,
    background_color: Color::Black,
};

pub const INVERSE_STYLE: FontStyle = FontStyle {
    font_style: rustbox::RB_NORMAL,
    foreground_color: Color::Black,
    background_color: Color::White,
};

/**
 * Data types
 */
pub const KEYWORD_TYPE: FontStyle = FontStyle {
    font_style: rustbox::RB_NORMAL,
    foreground_color: Color::Yellow,
    background_color: Color::Black,
};

/**
 * Module imports
 */
pub const KEYWORD_IMPORT: FontStyle = FontStyle {
    font_style: rustbox::RB_NORMAL,
    foreground_color: Color::Cyan,
    background_color: Color::Black,
};

/**
 * Loop constructs (for, while, etc.)
 */
pub const KEYWORD_LOOP: FontStyle = FontStyle {
    font_style: rustbox::RB_NORMAL,
    foreground_color: Color::Blue,
    background_color: Color::Black,
};

/**
 * Comments
 */
pub const COMMENT: FontStyle = FontStyle {
    font_style: rustbox::RB_NORMAL,
    foreground_color: Color::Magenta,
    background_color: Color::Black,
};

/**
 * Strings
 */
pub const STRING: FontStyle = FontStyle {
    font_style: rustbox::RB_NORMAL,
    foreground_color: Color::Red,
    background_color: Color::Black,
};

/**
 * Numbers
 */
/*
pub const NUMBER: FontStyle = FontStyle {
    font_style: rustbox::RB_NORMAL,
    foreground_color: Color::Cyan,
    background_color: Color::Black,
};
*/

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Coverage {
    // select from start to end
    FromTo,
    Word,
    Line,
}

impl Coverage {
    pub fn iter() -> Iter<'static, Coverage> {
        static COVERAGE_ALL: [Coverage; 3] = [Coverage::FromTo, Coverage::Word, Coverage::Line];
        COVERAGE_ALL.iter()
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum UndoRedoAction {
    Undo,
    Redo,
}

impl UndoRedoAction {
    pub fn iter() -> Iter<'static, UndoRedoAction> {
        static UNDOREDO_ALL: [UndoRedoAction; 2] = [UndoRedoAction::Undo, UndoRedoAction::Redo];
        UNDOREDO_ALL.iter()
    }
}

/**
 * Representation of a rectangular window
 */
#[derive(Copy, Clone, Debug)]
pub struct Buffer {
    // size of buffer
    pub size: Size,

    // editor location start
    pub editor_top_left: Location,
    // size available for editing
    pub editor_size: Size,
}

#[derive(Copy, Clone, Debug)]
pub struct Size {
    pub rows: usize,
    pub columns: usize,
}

impl Size {
    pub fn new(rows: usize, columns: usize) -> Self {
        Self { rows, columns }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Location {
    pub row: usize,
    pub column: usize,
}

impl Location {
    pub fn new(row: usize, column: usize) -> Self {
        Self { row, column }
    }
}
