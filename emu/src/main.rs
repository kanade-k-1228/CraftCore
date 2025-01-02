mod hooks;
mod model;

use clap::Parser;

use hooks::{dump::Dump, intr::Intr, serial::Serial, Hook};
use model::State;

const HELP_TEMPLATE: &str = "\
{before-help}{bin} {version}
  {author}
  {about}

{usage-heading}
{tab}{usage}

{all-args}{after-help}";

#[derive(Parser, Debug)]
#[clap(author, version, about,help_template = HELP_TEMPLATE)]
struct Args {
    #[arg(short = 't', long)]
    tmax: Option<u64>,

    #[arg(short, long)]
    dump_cfg: Option<String>,

    #[arg(short = 'a', long)]
    dump_all: bool,

    #[arg(short, long)]
    intr_cfg: Option<String>,

    #[arg(default_value = "main.rk.bin")]
    input_file: String,

    #[arg(long = "sin")]
    serial_in: Option<String>,

    #[arg(long = "sout")]
    serial_out: Option<String>,
}

fn main() {
    let args = Args::parse();
    println!("RK16 Emulator by kanade-k-1228");

    println!("+-----------------------------------------------+");
    println!("| {:<45} |", args.input_file);
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
    println!("[INIT]");
    let mut hooks: Vec<Box<dyn Hook>> = vec![
        Box::new(Dump::arg(args.dump_cfg, args.dump_all)),
        Box::new(Intr::arg(args.intr_cfg)),
        Box::new(Serial::arg(true, args.serial_out, args.serial_in)),
    ];

    // Apply initializations
    for hook in hooks.iter_mut() {
        state = hook.init(state);
    }

    // ------------------------------------------------------------------------
    // Main loop
    for time in match args.tmax {
        Some(t) => 0_u64..t,
        None => 0_u64..u64::MAX,
    } {
        // Execute instruction
        let (addr, code, _op, inst) = state.exec();
        print!("[{:0>4}] {}", time, inst.cformat());

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
}
