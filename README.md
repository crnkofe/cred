# Crusty editor

A Linux WYSIWYG console editor loosely inspired by ye-olde Turbo Pascal interface.
This editor makes most commands available to left hand only since right hand is way overused in development 
and avoids piano 2+ keys shortcuts.

To run
```bash
cargo r
```

# Features

## File operations

Open and close main menu by pressing *ESC* or *CTRL+d*. To move around press *TAB* or use arrows keys.
Some options have shortcuts. When the menu is opened hit the corresponding key in ( )/

From main menu you can Create, Save, Open or Quit.

## Editing

Text input is WYSIWYG. 

Common functionalities:
* copy - press *CTRL+c*, then *Tab* to toggle between different modes, *Space* to select start of copy area and *Enter* to end copy
* paste - press *CTRL+v*
* undo - press *CTRL+z* and *Enter*
* redo - press *CTRL+z*, *Tab* and *Enter*
* search - press *CTRL+f*, type in text, *Enter* then *Enter* or *f* to search forward or *b* to search backward

## Syntax Highlighting

Rust and Python have partial support.

