use core::fmt;
use std::io;

use kind_report::{
    report::{FileCache, Report},
    RenderConfig,
};

struct ToWriteFmt<T>(pub T);

impl<T> fmt::Write for ToWriteFmt<T>
where
    T: io::Write,
{
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0.write_all(s.as_bytes()).map_err(|_| fmt::Error)
    }
}

pub fn render_to_stderr<T, E>(render_config: &RenderConfig, session: &T, err: &E)
where
    T: FileCache,
    E: Report,
{
    Report::render(
        err,
        session,
        render_config,
        &mut ToWriteFmt(std::io::stderr()),
    )
    .unwrap();
}
