use clap::Parser;

fn main() {
    let mut args = tasm::cli::Args::parse();

    // project.yaml (共通定義: proj) が明示引数のないデフォルトを与える
    let cfg = proj::Config::load_or_default().unwrap_or_else(|e| {
        eprintln!("error: {:#}", e);
        std::process::exit(1);
    });
    if args.src.is_empty() {
        args.src = cfg.src;
    }
    if args.include.is_empty() {
        // include のキーがモジュールルート名になる
        args.include.extend(
            cfg.include
                .into_iter()
                .map(|(name, dir)| format!("{}={}", name, dir)),
        );
    }

    if let Err(e) = tasm::cli::run(&args) {
        eprintln!("error: {}", e);
        std::process::exit(1);
    }
}
