use std::io::Write;

use super::Hook;
use crate::model::State;

pub struct Video {
    dump_file: Option<String>,
    last_snapshot: Option<Vec<u16>>,
}

impl Video {
    pub const BEGIN: u16 = 0x1000;
    pub const END: u16 = 0x2FFF;

    pub fn arg(dump_file: Option<String>) -> Self {
        Self {
            dump_file,
            last_snapshot: None,
        }
    }

    fn snapshot(state: &State) -> Vec<u16> {
        let mut buf = Vec::with_capacity((Self::END - Self::BEGIN + 1) as usize);
        for addr in Self::BEGIN..=Self::END {
            buf.push(state.get(addr));
        }
        buf
    }
}

impl Hook for Video {
    fn init(&mut self, mut state: State) -> State {
        for addr in Self::BEGIN..=Self::END {
            state.set(addr, 0);
        }
        println!(
            " * VRAM: {:0>4X}-{:0>4X} ({} words)",
            Self::BEGIN,
            Self::END,
            Self::END - Self::BEGIN + 1
        );
        state
    }

    fn exec(&mut self, _time: u64, _addr: u16, _code: u32, state: State) -> State {
        if self.dump_file.is_some() {
            self.last_snapshot = Some(Self::snapshot(&state));
        }
        state
    }
}

impl Drop for Video {
    fn drop(&mut self) {
        if let (Some(path), Some(snap)) = (self.dump_file.as_ref(), self.last_snapshot.as_ref()) {
            if let Ok(file) = std::fs::File::create(path) {
                let mut w = std::io::BufWriter::new(file);
                for word in snap {
                    let _ = w.write_all(&word.to_le_bytes());
                }
                let _ = w.flush();
            }
        }
    }
}
