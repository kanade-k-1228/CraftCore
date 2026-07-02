use anyhow::Result;

use crate::hooks::{dump::Dump, intr::Intr, serial::Serial, video::Video, Hook};
use crate::model::State;

#[derive(Debug, clap::Args)]
pub struct Args {
    #[arg(short = 't', long)]
    pub tmax: Option<u64>,

    #[arg(short, long)]
    pub dump_cfg: Option<String>,

    #[arg(short = 'a', long)]
    pub dump_all: bool,

    #[arg(short, long)]
    pub intr_cfg: Option<String>,

    #[arg(default_value = "out/main.bin")]
    pub ibin: String,

    #[arg(default_value = "out/const.bin")]
    pub cbin: String,

    /// Serial input file
    #[arg(long)]
    pub sin: Option<String>,

    /// Serial output file
    #[arg(long)]
    pub sout: Option<String>,

    /// VRAM dump file (raw little-endian u16, 0x1000-0x2FFF)
    #[arg(long)]
    pub vram_out: Option<String>,
}

pub fn run(args: &Args) -> Result<()> {
    println!("+-----------------------------------------------+");
    println!("| {:<45} |", args.ibin);
    println!("+-----------------------------------------------+");

    // ------------------------------------------------------------------------
    // Initialize state machine

    let mut state = State::new();
    state.load_imem(&args.ibin)?; // 命令ファイルをロード
    state.load_dmem(&args.cbin)?; // 定数値ファイルをロード

    // ------------------------------------------------------------------------
    // Initialize hooks

    println!("[INIT]");

    let mut hooks: Vec<Box<dyn Hook>> = vec![
        Box::new(Serial::arg(true, args.sout.clone(), args.sin.clone())),
        Box::new(Video::arg(args.vram_out.clone())),
        Box::new(Intr::arg(args.intr_cfg.clone())),
        Box::new(Dump::arg(args.dump_cfg.clone(), args.dump_all)),
    ];

    for hook in hooks.iter_mut() {
        state = hook.init(state);
    }

    // ------------------------------------------------------------------------
    // Main loop

    let tmax = args.tmax.unwrap_or(u64::MAX);
    for time in 0..tmax {
        // 1. Execute instruction
        let (addr, code, op) = state.exec();
        println!("[{:0>4}] PC={:04X} {}", time, addr, op.print());

        // Execute side effects
        for hook in hooks.iter_mut() {
            state = hook.exec(time, addr, code, state);
        }

        if state.halt() {
            break;
        }
    }

    // Exit
    println!("=================================================");
    Ok(())
}
