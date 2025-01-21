extern crate iz80;
extern crate miniz_oxide;

mod ace_machine;
mod characters;
mod display;
mod runner;



pub use runner::Runner as Runner;
pub use ace_machine::MAX_INPUT_BUFFER_SIZE as MAX_INPUT_BUFFER_SIZE;

pub const GRAPH_CHARS: &str = "■ ▝ ▘ ▀ ▗ ▐ ▚ ▜ ▙ ▟ ▄ ▛ ▌ ▞ ▖";