use crate::parser::Line;
use color_print::cprintln;

#[derive(Debug)]
pub struct Msg<'a> {
    kind: MsgKind,
    msg: String,
    line: &'a Line<'a>,
}

#[derive(Debug)]
enum MsgKind {
    Error,
    Warn,
    Note,
}

impl<'a> Msg<'a> {
    pub fn error(msg: String, line: &'a Line) -> Self {
        Msg {
            kind: MsgKind::Error,
            msg,
            line,
        }
    }
    pub fn warn(msg: String, line: &'a Line) -> Self {
        Msg {
            kind: MsgKind::Warn,
            msg,
            line,
        }
    }
    pub fn note(msg: String, line: &'a Line) -> Self {
        Msg {
            kind: MsgKind::Note,
            msg,
            line,
        }
    }
}

impl Msg<'_> {
    fn print(&self) {
        match self.kind {
            MsgKind::Error => cprintln!("<red,bold>error</>: {}", self.msg),
            MsgKind::Warn => cprintln!("<yellow,bold>warn</>: {}", self.msg),
            MsgKind::Note => cprintln!("<green,bold>note</>: {}", self.msg),
        }
        cprintln!("     <blue>--></> <underline>{}</>", self.line.pos());
        cprintln!("      <blue>|</>");
        cprintln!(" <blue>{:>4} |</> {}", self.line.line_no(), self.line.raw());
        cprintln!("      <blue>|</>");
    }
}

pub fn dump(msgs: &Vec<Msg>) {
    for msg in msgs {
        msg.print();
    }
}

pub fn has_error(msgs: &Vec<Msg>) -> bool {
    if let Some(_) = msgs.iter().find(|msg| match msg.kind {
        MsgKind::Error => true,
        _ => false,
    }) {
        true
    } else {
        false
    }
}
