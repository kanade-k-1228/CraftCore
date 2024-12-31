mod hooks;
mod model;

use clap::Parser;

use hooks::{intr::Intr, print::Print, serial::Serial, Hook};
use model::State;

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

    #[arg(default_value = "main.rk.bin")]
    input_file: String,
}

fn main() {
    let args = Args::parse();
    println!("RK16 Emulator by kanade-k-1228");

    println!("+-----------------------------------------------+");
    println!("| Emulate: {:<36} |", args.input_file);
    if let Some(fname) = &args.print_cfg {
        println!("|  - Print: {:<35} |", fname);
    }
    if args.print_all {
        println!("|  - Print: {:<35} |", "All");
    }
    if let Some(fname) = &args.intr_cfg {
        println!("|  - Interrupt: {:<31} |", fname);
    }
    println!("+-----------------------------------------------+");

    // ------------------------------------------------------------------------
    // Initialize state machine model
    let mut state = {
        let mut cpu = State::new();
        cpu.load_rom_file(&args.input_file).unwrap();
        cpu
    };

    // ------------------------------------------------------------------------
    // Initialize hooks
    let mut hooks: Vec<Box<dyn Hook>> = vec![
        Box::new(Print::arg(args.print_cfg, args.print_all)),
        Box::new(Intr::arg(args.intr_cfg)),
        Box::new(Serial::new()),
    ];
    // Apply initializations
    state = hooks.iter_mut().fold(state, |state, hook| hook.init(state));

    // ------------------------------------------------------------------------
    // Main loop
    for time in match args.tmax {
        Some(t) => 0_u64..t,
        None => 0_u64..u64::MAX,
    } {
        // Execute instruction
        let (addr, code) = state.exec(time);
        // Execute side effects
        state = hooks
            .iter_mut()
            .fold(state, |state, hook| hook.exec(time, addr, code, state));
        if state.is_terminated() {
            break;
        }
    }

    // Exit
    println!("=================================================");
}
