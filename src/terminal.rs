use std::io::{self, Write};

use codespan_reporting::term::termcolor;
use termcolor::{ColorChoice, WriteColor};

// use crate::ARGS;

/// Returns a handle to the optionally colored terminal output.
pub fn out() -> TermOut {
    TermOut {
        stream: termcolor::StandardStream::stderr(ColorChoice::Auto),
    }
}

/// A utility that allows users to write colored terminal output.
/// If colors are not supported by the terminal, they are disabled.
/// This type also allows for deletion of previously written lines.
pub struct TermOut {
    stream: termcolor::StandardStream,
}

impl TermOut {
    /// Clears the entire screen when output supports terminal control.
    #[cfg(feature = "library-compiler")]
    pub fn clear_screen(&mut self) -> io::Result<()> {
        if self.stream.supports_color() {
            let mut stream = self.stream.lock();
            write!(stream, "\x1B[2J\x1B[1;1H")?;
            stream.flush()?;
        }
        Ok(())
    }
}

impl Write for TermOut {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.stream.lock().write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.stream.lock().flush()
    }
}

impl WriteColor for TermOut {
    fn supports_color(&self) -> bool {
        self.stream.supports_color()
    }

    fn set_color(&mut self, spec: &termcolor::ColorSpec) -> io::Result<()> {
        self.stream.lock().set_color(spec)
    }

    fn reset(&mut self) -> io::Result<()> {
        self.stream.lock().reset()
    }
}
