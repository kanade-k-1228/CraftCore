mod computer;
mod hooks;

use clap::Parser;

use computer::{Hook, State};
use hooks::{intr::Intr, print::Print, serial::Serial};

#[derive(Parser, Debug)]
#[clap(
    name = "RK16 Emulator",
    author = "kanade-k-1228",
    version = "v1.0.0",
    about = "Emulator for RK16 ISA"
)]
struct Args {
    #[arg(short = 't', long)]
    tmax: Option<u64>,

    #[arg(short, long)]
    print_cfg: Option<String>,

    #[arg(short = 'a', long)]
    print_all: bool,

    #[arg(short, long)]
    intr_cfg: Option<String>,

    #[arg(default_value = "out.rk.bin")]
    input_file: String,
}

fn main() {
    let args = Args::parse();
    println!("RK16 Emulator by kanade-k-1228");

    println!("+-----------------------------------------------+");
    println!("| Emulate: {:<37} |", args.input_file);
    if let Some(fname) = &args.print_cfg {
        println!("|  - Print: {:<37} |", fname);
    }
    if args.print_all {
        println!("|  - Print: {:<37} |", "All");
    }
    if let Some(fname) = &args.intr_cfg {
        println!("|  - Interrupt: {:<37} |", fname);
    }
    println!("+-----------------------------------------------+");

    // ------------------------------------------------------------------------

    // Hooks
    let mut print = Print::arg(args.print_cfg, args.print_all);
    let mut intr = Intr::arg(args.intr_cfg);
    let mut serial = Serial::new();

    // Computer Model
    let mut state = {
        let mut cpu = State::new();
        cpu.load_rom_file(&args.input_file).unwrap();
        cpu
    };

    // ------------------------------------------------------------------------
    // Main loop
    for time in match args.tmax {
        Some(t) => 0_u64..t,
        None => 0_u64..u64::MAX,
    } {
        let (addr, code) = state.exec();
        state = print.exec(time, addr, code, state);
        state = intr.exec(time, addr, code, state);
        state = serial.exec(time, addr, code, state);
        if state.shutdown {
            break;
        }
    }

    // 終了処理
    println!();
    println!("=================================================");
}
