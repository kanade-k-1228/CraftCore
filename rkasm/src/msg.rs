use crate::parser::Line;
use color_print::cprintln;

// ----------------------------------------------------------------------------

#[derive(Debug)]
pub struct Msgs(Vec<Msg>);

impl Msgs {
    pub fn new() -> Self {
        Msgs(Vec::new())
    }

    pub fn extend(&mut self, msgs: Msgs) {
        self.0.extend(msgs.0);
    }

    pub fn error(&mut self, msg: String, line: Line) {
        self.0.push(Msg {
            kind: MsgKind::Error,
            msg,
            line,
        });
    }

    pub fn warn(&mut self, msg: String, line: Line) {
        self.0.push(Msg {
            kind: MsgKind::Warn,
            msg,
            line,
        });
    }

    pub fn note(&mut self, msg: String, line: Line) {
        self.0.push(Msg {
            kind: MsgKind::Note,
            msg,
            line,
        });
    }

    pub fn flush(&mut self) {
        for msg in &self.0 {
            msg.print();
        }
        self.0.clear();
    }
}

// ----------------------------------------------------------------------------

#[derive(Debug)]
struct Msg {
    kind: MsgKind,
    msg: String,
    line: Line,
}

#[derive(Debug)]
enum MsgKind {
    Error,
    Warn,
    Note,
}

impl Msg {
    fn print(&self) {
        match self.kind {
            MsgKind::Error => cprintln!("<red,bold>error</>: {}", self.msg),
            MsgKind::Warn => cprintln!("<yellow,bold>warn</>: {}", self.msg),
            MsgKind::Note => cprintln!("<green,bold>note</>: {}", self.msg),
        }
        cprintln!("     <blue>--></> <underline>{}</>", self.line.pos());
        cprintln!("      <blue>|</>");
        cprintln!(" <blue>{:>4} |</> {}", self.line.no(), self.line.raw());
        cprintln!("      <blue>|</>");
    }
}
