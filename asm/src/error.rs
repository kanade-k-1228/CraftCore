use color_print::cprintln;
use indexmap::IndexMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Unknown operation: `{0}`")]
    UnknownOperation(String),

    #[error("More argument required")]
    MissingArgument,

    #[error("Cannot parse `{0}` as {1}")]
    ParseArgument(String, String),

    #[error("Syntax Error: Cannot parse")]
    SyntaxError,

    #[error("Undefined label: `{0}`")]
    UndefinedLabel(String),

    #[error("Re-defined label: `{0}`")]
    RedefinedLabel(String),

    #[error("Failed to open file: {0}")]
    FileOpen(String, #[source] std::io::Error),

    #[error("Failed to read line")]
    FileRead(#[source] std::io::Error),

    #[error("Failed to create file: {0}")]
    FileCreate(String, #[source] std::io::Error),

    #[error("Failed to write file: {0}")]
    FileWrite(String, #[source] std::io::Error),
}

impl Error {
    /// Print error with diagnostic information showing file location and line content
    pub fn print_diag(&self, files: &IndexMap<String, Vec<String>>, file: &str, line_idx: usize) {
        // Print the error message
        cprintln!("<red,bold>error</>: {}", self);

        // Print file location (line_idx is 0-based, display as 1-based)
        let line_num = line_idx + 1;
        cprintln!("     <blue>--></> <underline>{}:{}</>", file, line_num);
        cprintln!("      <blue>|</>");

        // Get the line content from the files map
        let line_content = files
            .get(file)
            .and_then(|lines| lines.get(line_idx))
            .map(|s| s.as_str())
            .unwrap_or("");

        cprintln!(" <blue>{:>4} |</> {}", line_num, line_content);
        cprintln!("      <blue>|</>");
    }
}
