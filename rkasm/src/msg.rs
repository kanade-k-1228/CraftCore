use color_print::cprintln;

#[derive(Debug)]
pub enum Msg {
    Error(String),
    Warn(String),
    Note(String),
}

impl Msg {
    pub fn print(&self) {
        match self {
            Msg::Error(msg) => cprintln!("<red,bold>error</>: {}", msg),
            Msg::Warn(msg) => cprintln!("<yellow,bold>warn</>: {}", msg),
            Msg::Note(msg) => cprintln!("<green,bold>note</>: {}", msg),
        }
    }
    pub fn diag(&self, info: (&str, usize, &str)) {
        let (file, line, raw) = info;
        self.print();
        cprintln!("     <blue>--></> <underline>{}:{}</>", file, line);
        cprintln!("      <blue>|</>");
        cprintln!(" <blue>{:>4} |</> {}", line, raw);
        cprintln!("      <blue>|</>");
    }
}
