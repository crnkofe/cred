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

#[cfg(test)]
mod tests {

    use std::path::PathBuf;
    use std::sync::Once;

    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use crate::cred::common;
    use crate::cred::controls::{rabin_karp_search, Editor, LineIndex};
    use crate::cred::events;

    use log::LevelFilter;
    use log4rs::append::console::ConsoleAppender;
    use log4rs::config::{Appender, Config, Root};
    use log4rs::encode::pattern::PatternEncoder;
    use std::error::Error;

    use crate::cred::events::HandleSearchEvent;
    use rustbox::{Event, ExtendedKey, Key, Modifiers, OutputMode};

    static LOG_INIT: Once = Once::new();

    pub fn setup() -> Result<(), Box<dyn Error>> {
        LOG_INIT.call_once(|| {
            // configure logging
            let logfile = ConsoleAppender::builder()
                .encoder(Box::new(PatternEncoder::new("{l} - {m}\n")))
                .build();

            let config = Config::builder()
                .appender(Appender::builder().build("console", Box::new(logfile)))
                .build(Root::builder().appender("console").build(LevelFilter::Info));

            log4rs::init_config(config.unwrap()).unwrap();
        });
        Ok(())
    }

    #[test]
    fn test_line_index_create() {
        assert_eq!(true, setup().is_ok());
        let index: LineIndex = LineIndex::new();
        assert_eq!(None, index.line(0));
        assert_eq!(0, index.len());
    }

    #[test]
    fn test_line_index_location() {
        assert_eq!(true, setup().is_ok());
        let mut index: LineIndex = LineIndex::new();
        for i in 1..4 {
            index.push_line(i);
        }
        assert_eq!(Some(0), index.location(0));
        assert_eq!(Some(1), index.location(1));
        assert_eq!(Some(3), index.location(2));
        assert_eq!(Some(3), index.location(100));
    }

    #[test]
    fn test_line_index_add() {
        assert_eq!(true, setup().is_ok());
        let mut index: LineIndex = LineIndex::new();
        for i in 1..4 {
            index.push_line(i);
        }
        assert_eq!(Some((0, 0)), index.line(0));
        assert_eq!(Some((1, 1)), index.line(1));
        assert_eq!(Some((1, 1)), index.line(2));
        assert_eq!(Some((2, 3)), index.line(3));
        assert_eq!(Some((2, 3)), index.line(4));
        assert_eq!(Some((2, 3)), index.line(5));

        assert_eq!(Some((2, 3)), index.line(100));
    }

    #[test]
    fn test_line_index_break_start() {
        assert_eq!(true, setup().is_ok());
        let mut index: LineIndex = LineIndex::new();
        index.push_line(10);
        index.break_line(0);
        assert_eq!(2, index.len());

        assert_eq!(Some((0, 0)), index.line(0));
        assert_eq!(Some((1, 1)), index.line(1));

        assert_eq!(Some(1), index.line_length(0));
        assert_eq!(Some(10), index.line_length(1));
    }

    #[test]
    fn test_line_index_break_mid() {
        assert_eq!(true, setup().is_ok());
        let mut index: LineIndex = LineIndex::new();
        index.push_line(10);
        index.break_line(5);
        assert_eq!(2, index.len());

        assert_eq!(Some((0, 0)), index.line(0));
        assert_eq!(Some((1, 6)), index.line(6));

        assert_eq!(Some(6), index.line_length(0));
        assert_eq!(Some(5), index.line_length(1));
    }

    #[test]
    fn test_line_index_break_before_end() {
        assert_eq!(true, setup().is_ok());
        let mut index: LineIndex = LineIndex::new();
        index.push_line(10);
        index.break_line(9);
        assert_eq!(2, index.len());

        assert_eq!(Some((0, 0)), index.line(0));
        assert_eq!(Some((1, 10)), index.line(10));

        assert_eq!(Some(10), index.line_length(0));
        assert_eq!(Some(1), index.line_length(1));
    }

    #[test]
    fn test_line_index_break_beyond_end() {
        assert_eq!(true, setup().is_ok());
        let mut index: LineIndex = LineIndex::new();
        index.push_line(10);
        index.break_line(10);
        assert_eq!(2, index.len());

        assert_eq!(Some((0, 0)), index.line(0));
        assert_eq!(Some((1, 10)), index.line(10));

        assert_eq!(Some(10), index.line_length(0));
        assert_eq!(Some(1), index.line_length(1));
    }

    #[test]
    fn test_line_index_insert_char() {
        assert_eq!(true, setup().is_ok());
        let mut index: LineIndex = LineIndex::new();
        index.push_line(10);
        assert_eq!(Some(10), index.line_length(0));
        index.insert_character_at(0);
        assert_eq!(Some(11), index.line_length(0));
        index.insert_character_at(1000);
        assert_eq!(Some(12), index.line_length(0));
    }

    #[test]
    fn test_line_index_remove_char() {
        assert_eq!(true, setup().is_ok());
        let mut index: LineIndex = LineIndex::new();
        index.push_line(10);
        assert_eq!(Some(10), index.line_length(0));
        index.remove_character_at(0);
        assert_eq!(Some(9), index.line_length(0));
    }

    #[test]
    fn test_line_index_remove_line() {
        assert_eq!(true, setup().is_ok());
        let mut index: LineIndex = LineIndex::new();
        for i in 1..4 {
            index.push_line(i);
        }
        assert_eq!(3, index.len());
        index.remove_line_at(6);
        assert_eq!(3, index.len());
        index.remove_line_at(5);
        assert_eq!(3, index.len());
        index.remove_line_at(2);
        assert_eq!(2, index.len());
        assert_eq!(Some(1), index.line_length(0));
        assert_eq!(Some(4), index.line_length(1)); //  4 because newline is "removed" on merging
        index.remove_line_at(0);
        assert_eq!(Some(4), index.line_length(0));
    }

    #[test]
    fn test_rabin_karp() {
        assert_eq!(true, setup().is_ok());
        let test_string = "Lorem ipsum dolor sit amet,\
        consectetur adipiscing elit, sed do eiusmod tempor \
        incididunt ut labore et dolore magna aliqua. \
        Ut enim ad minim veniam, quis nostrud exercitation \
        ullamco laboris nisi ut aliquip ex ea commodo consequat. \
        Duis aute irure dolor in reprehenderit in voluptate velit \
        esse cillum dolore eu fugiat nulla pariatur. Excepteur sint \
        occaecat cupidatat non proident, sunt in culpa qui officia \
        deserunt mollit anim id est laborum.";
        let text: Vec<char> = test_string.bytes().map(|x| x as char).collect();
        let pattern = "ut";

        let result = rabin_karp_search(pattern.to_string(), &text, 7);
        assert_eq!(3, result.len());
    }

    #[test]
    fn test_editor_create_file() {
        assert_eq!(true, setup().is_ok());
        let mut editor = Editor::new();
        assert_eq!(true, editor.init(OutputMode::NoOutput).is_ok());
        // initially one buffer is created
        assert_eq!(1, editor.file_buffer_controls.len());
    }

    #[test]
    fn test_editor_edit_file() {
        assert_eq!(true, setup().is_ok());
        let mut editor = Editor::new();
        assert_eq!(true, editor.init(OutputMode::NoOutput).is_ok());

        let expected_result = String::from("Lorem ipsum dolor sit amet");
        for c in expected_result.chars() {
            let event = Event::KeyEvent(ExtendedKey::new(Key::Char(c), Modifiers::new()));
            assert_eq!(true, editor.handle_event_option(Some(Ok(event))));
        }

        let file_buffer = editor.get_active_file_buffer().unwrap();
        let slice = file_buffer.get_slice_string(0, 100);
        assert_eq!("Lorem ipsum dolor sit amet", slice);
    }

    #[test]
    fn test_editor_open_file() {
        assert_eq!(true, setup().is_ok());
        let mut editor = Editor::new();
        assert_eq!(true, editor.init(OutputMode::NoOutput).is_ok());

        let open_this_test = events::Event {
            open_file_event: Some(events::OpenFileEvent {
                path: PathBuf::from("./src/cred/tests.rs"),
            }),
            ..events::Event::new()
        };
        assert_eq!(true, editor.handle_event(open_this_test));
        assert_eq!(2, editor.file_buffer_controls.len());

        let file_buffer = editor.get_active_file_buffer().unwrap();
        let slice = file_buffer.get_slice_string(0, 18);
        assert_eq!("/**\n * MIT License", slice);
    }

    #[test]
    fn test_editor_exit() {
        assert_eq!(true, setup().is_ok());
        let mut editor = Editor::new();
        assert_eq!(true, editor.init(OutputMode::NoOutput).is_ok());

        let event_menu = Event::KeyEvent(ExtendedKey::new(
            Key::Char('d'),
            Modifiers {
                ctrl: true,
                ..Modifiers::new()
            },
        ));
        let exit_shortcut = Event::KeyEvent(ExtendedKey::new(Key::Char('x'), Modifiers::new()));

        assert_eq!(true, editor.handle_event_option(Some(Ok(event_menu))));
        assert_eq!(false, editor.handle_event_option(Some(Ok(exit_shortcut))));
    }

    #[ignore]
    #[test]
    fn test_editor_copy_paste() {
        assert_eq!(true, setup().is_ok());
        let mut editor = Editor::new();
        assert_eq!(true, editor.init(OutputMode::NoOutput).is_ok());
        assert_eq!(1, editor.file_buffer_controls.len());

        let file_buffer = editor.get_active_file_buffer_mut().unwrap();
        let empty_slice = file_buffer.get_slice_string(0, 5);
        assert_eq!("", empty_slice);

        let contents: Vec<char> = String::from("test").chars().map(|x| x as char).collect();
        file_buffer.to_clipboard(contents);
        file_buffer.from_clipboard();

        let slice = file_buffer.get_slice_string(0, 5);
        assert_eq!("test", slice);
    }

    #[test]
    fn test_editor_search() {
        assert_eq!(true, setup().is_ok());
        let mut editor = Editor::new();
        assert_eq!(true, editor.init(OutputMode::NoOutput).is_ok());

        let open_this_test = events::Event {
            open_file_event: Some(events::OpenFileEvent {
                path: PathBuf::from("./src/cred/tests.rs"),
            }),
            ..events::Event::new()
        };
        assert_eq!(true, editor.handle_event(open_this_test));
        assert_eq!(2, editor.file_buffer_controls.len());

        let file_buffer = editor.get_active_file_buffer_mut().unwrap();
        let search_event = events::SearchEvent {
            direction: events::SearchDirection::Forward,
            pattern: "test_editor_search".to_string(),
        };

        file_buffer.handle_search_event(search_event, common::Buffer::new(100, 100));
        let match_location = file_buffer.get_current_match_location();
        assert_eq!(true, match_location.is_some());
        assert_eq!(true, match_location.unwrap() > 0);
    }

    #[test]
    fn test_editor_undo() {
        assert_eq!(true, setup().is_ok());
        let mut editor = Editor::new();
        assert_eq!(true, editor.init(OutputMode::NoOutput).is_ok());

        let expected_result = String::from("Lorem ipsum dolor sit amet");
        for c in expected_result.chars() {
            let event = Event::KeyEvent(ExtendedKey::new(Key::Char(c), Modifiers::new()));
            assert_eq!(true, editor.handle_event_option(Some(Ok(event))));
        }

        let undo_overlay_menu = Event::KeyEvent(ExtendedKey::new(
            Key::Char('z'),
            Modifiers {
                ctrl: true,
                ..Modifiers::new()
            },
        ));
        let undo_ok = Event::KeyEvent(ExtendedKey::new(Key::Enter, Modifiers::new()));

        assert_eq!(
            true,
            editor.handle_event_option(Some(Ok(undo_overlay_menu)))
        );
        assert_eq!(true, editor.handle_event_option(Some(Ok(undo_ok))));

        let file_buffer = editor.get_active_file_buffer_mut().unwrap();
        let slice = file_buffer.get_slice_string(0, 5);
        assert_eq!("", slice);
    }

    #[test]
    fn test_editor_redo() {
        assert_eq!(true, setup().is_ok());
        let mut editor = Editor::new();
        assert_eq!(true, editor.init(OutputMode::NoOutput).is_ok());

        let expected_result = String::from("Lorem ipsum dolor sit amet");
        for c in expected_result.chars() {
            let event = Event::KeyEvent(ExtendedKey::new(Key::Char(c), Modifiers::new()));
            assert_eq!(true, editor.handle_event_option(Some(Ok(event))));
        }

        let undo_overlay_menu = Event::KeyEvent(ExtendedKey::new(
            Key::Char('z'),
            Modifiers {
                ctrl: true,
                ..Modifiers::new()
            },
        ));

        let undo_ok = Event::KeyEvent(ExtendedKey::new(Key::Enter, Modifiers::new()));
        assert_eq!(
            true,
            editor.handle_event_option(Some(Ok(undo_overlay_menu)))
        );
        assert_eq!(true, editor.handle_event_option(Some(Ok(undo_ok))));

        let redo = Event::KeyEvent(ExtendedKey::new(Key::Tab, Modifiers::new()));
        let redo_ok = Event::KeyEvent(ExtendedKey::new(Key::Enter, Modifiers::new()));
        assert_eq!(
            true,
            editor.handle_event_option(Some(Ok(undo_overlay_menu)))
        );
        assert_eq!(true, editor.handle_event_option(Some(Ok(redo))));
        assert_eq!(true, editor.handle_event_option(Some(Ok(redo_ok))));

        let file_buffer = editor.get_active_file_buffer_mut().unwrap();
        let slice = file_buffer.get_slice_string(0, 50);
        assert_eq!(expected_result, slice);
    }
}
