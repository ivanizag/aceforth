extern crate iz80;
extern crate miniz_oxide;

mod ace_machine;
mod characters;
mod display;
mod runner;

pub use ace_machine::MAX_INPUT_BUFFER_SIZE;
pub use runner::Runner;

pub const GRAPH_CHARS: &str = "■ ▝ ▘ ▀ ▗ ▐ ▚ ▜ ▙ ▟ ▄ ▛ ▌ ▞ ▖";
