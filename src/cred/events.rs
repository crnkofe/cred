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
use rustbox::ExtendedKey;
use std::path::PathBuf;

use uuid::Uuid;

use super::common::Buffer;
use super::common::ControlType;
use super::common::Coverage;
use super::common::UndoRedoAction;

#[derive(Clone, Debug)]
pub enum WindowAction {
    Open,
    Close,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DialogType {
    Save,
    Goto,
}

#[derive(Clone, Debug)]
pub struct WindowEvent {
    pub action: WindowAction,
    pub control_type: ControlType,
    pub dialog_type: Option<DialogType>,
    pub control_id: Option<Uuid>,
}

impl WindowEvent {
    pub fn open(control_type: ControlType) -> Self {
        Self {
            action: WindowAction::Open,
            control_type,
            dialog_type: None,
            control_id: None,
        }
    }

    pub fn open_dialog(control_type: ControlType, dialog_type: DialogType) -> Self {
        Self {
            action: WindowAction::Open,
            control_type,
            dialog_type: Some(dialog_type),
            control_id: None,
        }
    }

    pub fn close(control_type: ControlType, control_id: Option<Uuid>) -> Self {
        Self {
            action: WindowAction::Close,
            control_type,
            dialog_type: None,
            control_id,
        }
    }
}

#[derive(Clone, Debug)]
pub struct UndoEvent {
    pub action: UndoRedoAction,
}

#[derive(Clone, Debug)]
pub struct ExitEvent {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SearchDirection {
    Forward,
    Backward,
}

#[derive(Clone, Debug)]
pub struct SearchEvent {
    pub direction: SearchDirection,
    pub pattern: String,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum SelectAction {
    // user selected start of selection
    Start,
    // user
    End,
}

#[derive(Clone, Debug)]
pub struct SelectEvent {
    pub action: SelectAction,
    pub coverage: Coverage,
}

#[derive(Clone, Debug)]
pub struct CreateFileEvent {}

#[derive(Clone, Debug)]
pub struct SaveFileEvent {
    pub path: PathBuf,
}

#[derive(Clone, Debug)]
pub struct OpenFileEvent {
    pub path: PathBuf,
}

#[derive(Clone, Debug)]
pub struct GotoLineEvent {
    pub line: usize,
}

#[derive(Clone, Debug)]
pub struct Event {
    pub handled: bool,
    pub bubble_down: bool,
    pub create_file_event: Option<CreateFileEvent>,
    pub save_file_event: Option<SaveFileEvent>,
    pub open_file_event: Option<OpenFileEvent>,
    pub key: Option<ExtendedKey>,
    pub select_event: Option<SelectEvent>,
    pub undo_event: Option<UndoEvent>,
    pub search_event: Option<SearchEvent>,
    pub window_event: Option<WindowEvent>,
    pub exit_event: Option<ExitEvent>,
    pub gotoline_event: Option<GotoLineEvent>,
}

impl Event {
    pub fn new() -> Self {
        Self {
            handled: false,
            bubble_down: false,
            create_file_event: None,
            save_file_event: None,
            open_file_event: None,
            select_event: None,
            undo_event: None,
            key: None,
            search_event: None,
            window_event: None,
            exit_event: None,
            gotoline_event: None,
        }
    }

    pub fn key(key: ExtendedKey) -> Self {
        Self {
            handled: false,
            bubble_down: false,
            create_file_event: None,
            save_file_event: None,
            open_file_event: None,
            select_event: None,
            undo_event: None,
            key: Some(key),
            search_event: None,
            window_event: None,
            exit_event: None,
            gotoline_event: None,
        }
    }
}

pub trait HandleSelectEvent {
    fn handle_select_event(&mut self, _select_event: SelectEvent) -> Event {
        Event::new()
    }
}

pub trait HandleUndoEvent {
    fn handle_undo_event(&mut self, _undo_event: UndoEvent, _window_buffer: Buffer) -> Event {
        Event::new()
    }
}

pub trait HandleEvent {
    fn handle_event(&self, _search_event: Event) -> Event {
        Event::new()
    }
}

pub trait HandleSearchEvent {
    fn handle_search_event(&mut self, _search_event: SearchEvent, _window_buffer: Buffer) -> Event {
        Event::new()
    }
}

pub trait HandleGotoEvent {
    fn handle_goto_event(&mut self, _goto_event: GotoLineEvent, _window_buffer: Buffer) -> Event {
        Event::new()
    }
}

pub trait HandleWindowEvent {
    fn handle_window_event(&mut self, window_event: WindowEvent) -> Event {
        match window_event.action {
            WindowAction::Open => self.handle_open_window_event(window_event),
            WindowAction::Close => self.handle_close_window_event(window_event),
        }
    }

    fn handle_open_window_event(&mut self, _window_event: WindowEvent) -> Event {
        Event::new()
    }

    fn handle_close_window_event(&mut self, _window_event: WindowEvent) -> Event {
        Event::new()
    }
}

/**
 * Common trait for all visual controls
 */
pub trait HandleKey {
    fn handle_key(&mut self, _key: ExtendedKey, _window_buffer: Buffer) -> Event {
        Event::new()
    }
}
