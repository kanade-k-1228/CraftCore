use std::path::Path;

use anyhow::{bail, Result};
use clap::Parser;

#[derive(clap::Parser)]
#[clap(author, version, about)]
struct Cli {
    /// project.yaml の test エントリ名 (省略時は "default")
    #[arg(long)]
    test: Option<String>,

    #[command(flatten)]
    args: cemu::cli::Args,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let mut args = cli.args;

    // project.yaml (共通定義: proj) の test エントリが明示引数のないデフォルトを与える
    let cfg = proj::Config::load_or_default()?;
    let test = match &cli.test {
        Some(name) => match cfg.test.get(name) {
            Some(t) => Some(t),
            None => bail!("test `{}` is not defined in {}", name, proj::FILE_NAME),
        },
        None => cfg.test.get("default"),
    };
    if let Some(t) = test {
        args.tmax = args.tmax.or(t.tmax);
        args.dump_all |= t.dump_all;
        args.sin = args.sin.or_else(|| t.sin.clone());
        args.sout = args.sout.or_else(|| t.sout.clone());
        args.vram_out = args.vram_out.or_else(|| t.vram_out.clone());
    }

    // カレントの dump.yaml / intr.yaml は指定がなくても自動で読む
    if args.dump_cfg.is_none() && Path::new("dump.yaml").exists() {
        args.dump_cfg = Some("dump.yaml".to_string());
    }
    if args.intr_cfg.is_none() && Path::new("intr.yaml").exists() {
        args.intr_cfg = Some("intr.yaml".to_string());
    }

    cemu::cli::run(&args)
}
