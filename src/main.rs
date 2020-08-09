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
extern crate unicode_segmentation;
extern crate clipboard;
extern crate lazy_static; // 1.0.1;

use rustbox::OutputMode;
use std::error::Error;
use log4rs::append::file::FileAppender;
use log4rs::encode::pattern::PatternEncoder;
use log4rs::config::{Appender, Config, Root};
use log::LevelFilter;
use std::panic;

mod cred;

// panic hook logs errors to file
pub fn panic_hook(info: &panic::PanicInfo) {
    let msg = info.to_string();
    log::error!("Crusty panic: {}", msg);
}

fn main() -> Result<(), Box<dyn Error>> {
    // configure logging
    let logfile = FileAppender::builder()
        .encoder(Box::new(PatternEncoder::new("{l} - {m}\n")))
        .build("log/cred.log")?;

    let config = Config::builder()
        .appender(Appender::builder().build("logfile", Box::new(logfile)))
        .build(Root::builder().appender("logfile").build(LevelFilter::Info))?;

    log4rs::init_config(config)?;
    panic::set_hook(Box::new(panic_hook));

    let mut editor = cred::controls::Editor::new();
    match editor.init(OutputMode::EightBit) {
        Ok(_) => { /* noop */ }
        Err(e) => {
            log::error!("Failed initializing editor: {:?}", e);
        }
    }

    editor.main_loop();
    Ok(())
}

