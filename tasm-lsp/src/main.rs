fn main() -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
    let (connection, io_threads) = lsp_server::Connection::stdio();
    tasm_lsp::run(connection)?;
    io_threads.join()?;
    Ok(())
}
