# Crusty editor

A Linux WYSIWYG console editor in Rust(!) loosely inspired by ye-olde Turbo Pascal interface.
This editor makes most commands available to left hand only since right hand is way overused in development 
and avoids piano 2+ keys shortcuts.

To run
```bash
cargo r
```

# Features

## File operations

Open and close main menu by pressing *ESC* or *CTRL+d*. To move around press *TAB* or use arrows keys.
Some options have shortcuts. When the menu is opened hit the corresponding key in brackets ( )

From main menu you can Create, Save, Open or Quit.

## Editing

Text input is WYSIWYG.

For help press F1.

Common editing functions:
* copy - press *CTRL+c*, then *Tab* to toggle between different modes, *Space* to select start of copy area and *Enter* to end copy
* paste - press *CTRL+v*
* undo - press *CTRL+z* and *Enter*
* redo - press *CTRL+z*, *Tab* and *Enter*
* search - press *CTRL+f*, type in text, *Enter* then *Enter* or *f* to search forward or *b* to search backward

To close any overlay press *ESC*

On Open File/Buffer windows:
* move around with arrow keys or w(up)/s(down)/a(left)/d(right)
* select file or buffer with *Space* or *Enter*

## Syntax Highlighting

Rust and Python have partial syntax highlighting support.

## Development

Make sure to run
```bash
. ./check.sh
```
before pushing and merging.

Github actions also trigger on each push. See .github/workflows/runtest.yml for details.

# TODO

Assorted ideas for future releases:
* show line numbers
* allow shortcut configuration
* handle large files gracefully
* run script (Python)
* run unit test (Python)
* compile Cargo project (Rust)
* run unit test (Rust)
* Git support
* figure out a better event system
* test thouroughly for random bugs
* add replace to search
* add support for themes
* complete existing syntax highlighters and add support for more
* add basic IDE features(compile, run unit tests, show documentation etc.)
* completion 
* multi-user editing support in console
