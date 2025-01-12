
pub const ERROR_CODE_NONE: u8 = 255;
pub const ERROR_CODE_UNKNOWN_METACOMMAND: u8 = 100;
pub const ERROR_CODE_QUIT: u8 = 101;
pub const ERROR_CODE_INPUT_BUFFER_OVERFLOW: u8 = 102;
pub const ERROR_CODE_HALT: u8 = 103;
pub const ERROR_CODE_TIMEOUT: u8 = 104;


pub fn error_message(code: u8) -> &'static str {
    match code {
        0 => "No error",
        1 => "Not enough memory",
        2 => "Data stack underflow",
        3 => "BREAK pressed",
        4 => "Compiling word in interpret mode",
        5 => "Word not properly structured",
        6 => "Name of new word empty or too long",
        7 => "PICK or ROLL used with operand zero or negative",
        8 => "Overflow in floating point arithmetic",
        9 => "Trying to print in input buffer",
        10 => "Tape error",
        11 => "Error in REDEFINE or FORGET",
        12 => "Incomplete definition in dictionary",
        13 => "Word not found (or is internal)",
        14 => "Word unlistable",
 
        // Additional error codes
        ERROR_CODE_NONE => "No error",
        ERROR_CODE_UNKNOWN_METACOMMAND => "Unknown metacommand",
        ERROR_CODE_QUIT => "Quit",
        ERROR_CODE_INPUT_BUFFER_OVERFLOW => "Input buffer overflow",
        ERROR_CODE_HALT => "Halt",
        ERROR_CODE_TIMEOUT => "Timeout",
        _ => "Unknown error",
    }
}


