use std::fmt;
use std::io;

/** custom error for file saving */
pub enum FileSaveError {
    Io(io::Error),
    NoFileSpecifiedError,
}

impl fmt::Display for FileSaveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FileSaveError::Io(ref cause) => write!(f, "I/O Error: {}", cause),
            FileSaveError::NoFileSpecifiedError => write!(f, "No file specified for saving!"),
        }
    }
}

impl From<io::Error> for FileSaveError {
    fn from(cause: io::Error) -> FileSaveError {
        FileSaveError::Io(cause)
    }
}
