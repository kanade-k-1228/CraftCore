mod hooks;
mod model;

use clap::Parser;

use hooks::{dump::Dump, intr::Intr, serial::Serial, Hook};
use model::State;

#[derive(Parser, Debug)]
#[clap(author, version, about)]
struct Args {
    #[arg(short = 't', long)]
    tmax: Option<u64>,

    #[arg(short, long)]
    dump_cfg: Option<String>,

    #[arg(short = 'a', long)]
    dump_all: bool,

    #[arg(short, long)]
    intr_cfg: Option<String>,

    #[arg(default_value = "main.bin")]
    ibin: String,

    #[arg(default_value = "const.bin")]
    cbin: String,

    #[arg(long = "sin")]
    serial_in: Option<String>,

    #[arg(long = "sout")]
    serial_out: Option<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    println!("RK16 Emulator by kanade-k-1228");

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
        Box::new(Serial::arg(true, args.serial_out, args.serial_in)),
        Box::new(Intr::arg(args.intr_cfg)),
        Box::new(Dump::arg(args.dump_cfg, args.dump_all)),
    ];

    for hook in hooks.iter_mut() {
        state = hook.init(state);
    }

    // ------------------------------------------------------------------------
    // Main loop

    let tmax = args.tmax.unwrap_or(u64::MAX);
    for time in 0..tmax {
        // Execute instruction
        let (addr, code, _, inst) = state.exec();
        println!("[{:0>4}] {}", time, inst.cformat());

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
