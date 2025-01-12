use std::fs;
use std::io;

use miniz_oxide::deflate::compress_to_vec;
use miniz_oxide::deflate::CompressionLevel;
use miniz_oxide::inflate::decompress_to_vec_with_limit;

use iz80::*;

use crate::ace_machine::AceMachine;
use crate::ace_machine::FLAG_ADDRESS;
use crate::ace_machine::MAX_INPUT_BUFFER_SIZE;
use crate::ace_machine::END_OF_ROM;
use crate::characters::{ace_to_emited, ace_to_screenshot};
use crate::errors::*;

static INITIAL_SNAPSHOT: &[u8] = include_bytes!("../resources/initialstate.sav");

pub struct Runner {
    cpu: Cpu,
    machine: AceMachine,
    trace_rom: bool,
    trace_on_timeout: bool,
    dump_screen: bool,
}

pub struct Response {
    pub output: String,
    pub pending_input: Option<String>,
    pub error_code: Option<u8>,
    pub screen: Option<String>,
}

const METACOMMAND_PREFIX: &str = "$";

const ROM_EMIT_CHAR_ADDRESS: u16 = 0x0008;
const ROM_RAISE_ERROR_ADDRESS: u16 = 0x0022;
const WAIT_FOR_SYNC_HALT_ADDRESS: u16 = 0x0679;
const SPINWAIT_FOR_LISTING_ADDRESS: u16 = 0x16e1;
const BEEP_LOOP_ADDRESS: u16 = 0x0bc1;
const QUERY_LOOP_ADDRESS: u16 = 0x059b;

impl Runner {
    pub fn new(trace_cpu: bool, trace_io: bool, trace_rom: bool) -> Runner {
        let machine = AceMachine::new(trace_io, true /* force invis */);
        let mut cpu = Cpu::new_z80();
        cpu.set_trace(trace_cpu);

        Runner {
            cpu,
            machine,
            trace_rom,
            trace_on_timeout: false,
            dump_screen: false,
        }
    }

    pub fn prepare(&mut self) {

        let res = self.load_snapshot_internal(INITIAL_SNAPSHOT);
        if res.is_err() {
            println!("Warning: Could not load initial state");
            // Advance to the query state
            while self.cpu.registers().pc() != QUERY_LOOP_ADDRESS {
                self.cpu.execute_instruction(&mut self.machine);
            }
        }

        // Touch the flags to ensure invis is properly set
        self.machine.poke(FLAG_ADDRESS, self.machine.peek(FLAG_ADDRESS));
    }

    pub fn execute(&mut self, commands: &str) -> Response {
        let mut response = self.execute_internal(commands);
        if self.dump_screen {
            response.screen = Some(self.machine.get_screen_as_text());
        }
        response
    }

    pub fn execute_internal(&mut self, commands: &str) -> Response {
        let mut outputs = Vec::new();
        let mut lines = commands.lines().collect::<Vec<_>>();
        if lines.is_empty() {
            // At least one line is considered
            lines.push("");
        }
        for line in lines {
            if line.len() > MAX_INPUT_BUFFER_SIZE as usize {
                return Response {
                    output: format!("Command line too long. The maximum supported size is {}", MAX_INPUT_BUFFER_SIZE),
                    pending_input: None,
                    error_code: Some(ERROR_CODE_INPUT_BUFFER_OVERFLOW),
                    screen: None,
                };
            }

            let response = if let Some(command) = line.strip_prefix(METACOMMAND_PREFIX) {
                if command.starts_with(" ") {
                    self.execute_command(line, 1_000_000_000)
                } else {
                    self.execute_metacommand(&line[METACOMMAND_PREFIX.len()..])
                }
            } else {
                self.execute_command(line, 1_000_000_000)
            };

            if response.error_code.is_some()
                    || response.pending_input.is_some()
                    || response.screen.is_some() {
                return response;
            }

            outputs.push(response.output);
        }

        Response {
            output: outputs.join("\n"),
            pending_input: None,
            error_code: None,
            screen: None,
        }
    }

    const METACOMMANDS_HELP: &str = r##"
Type the same command you would type in the Jupitar ACE prompt.

Additional metacommands are available starting with {prefix}:
  {prefix}HELP: Shows this help
  {prefix}QUIT: Exits the emulator
  {prefix}SCREENSHOT: Shows the current screen contents
  {prefix}SCREEN: Toggles screen dumping
  {prefix}TRACE: Toggles ROM tracing
  {prefix}SAVE [filename]: Saves a snapshot to a file
  {prefix}LOAD [filename]: Loads a snapshot from a file
  {prefix}GRAPHS: Show the Jupiter Ace graphical characters for easy copy-pasting.
  {prefix}VIS: Toggles invisible mode, Overwrites the VIS and INVIS words

"##;

    fn execute_metacommand(&mut self, command: &str) -> Response {
        let output;
        let pending_input = None;
        let mut error_code = None;
        let mut screen = None;

        let parts = command.split_whitespace().collect::<Vec<_>>();
        let command = parts[0].to_uppercase();

        match command.as_str() {
            "HELP" => {
                output = Self::METACOMMANDS_HELP.replace("{prefix}", METACOMMAND_PREFIX);
            },
            "QUIT" => {
                output = "".to_string();
                error_code = Some(ERROR_CODE_QUIT);
            },
            "SCREENSHOT" => {
                output = "".to_string();
                screen = Some(self.machine.get_screen_as_text());
            },
            "SCREEN" => {
                self.dump_screen = !self.dump_screen;
                output = format!("Screen dumping is now {}", if self.dump_screen { "enabled" } else { "disabled" });
            },
            "TRACE" => {
                self.trace_rom = !self.trace_rom;
                output = format!("ROM tracing is now {}", if self.trace_rom { "enabled" } else { "disabled" });
            },
            "SAVE" => {
                let filename = parts.get(1).unwrap_or(&"snapshot.sav");
                match self.save_snapshot(filename) {
                    Ok(_) => output = format!("Snapshot saved to {}", filename),
                    Err(e) => output = format!("Error saving snapshot: {}", e),
                }
            },
            "LOAD" => {
                let filename = parts.get(1).unwrap_or(&"snapshot.sav");
                match self.load_snapshot(filename) {
                    Ok(_) => output = format!("Snapshot loaded from {}", filename),
                    Err(e) => output = format!("Error loading snapshot: {}", e),
                }
            },
            "GRAPHS" => {
                output = "Graph characters: ■ ▝ ▘ ▀ ▗ ▐ ▚ ▜ ▙ ▟ ▄ ▛ ▌ ▞ ▖".to_string();
            },
            "VIS" => {
                let force_invis = !self.machine.force_invis;
                self.machine.force_invis = force_invis;
                if force_invis {
                    self.machine.poke(FLAG_ADDRESS, self.machine.peek(FLAG_ADDRESS) | 0x10);
                } else {
                    self.machine.poke(FLAG_ADDRESS, self.machine.peek(FLAG_ADDRESS) & !0x10);
                }
                output = format!("Invisible mode is now {}", if force_invis { "forced" } else { "not forced" });
            },
            _ => {
               error_code = Some(ERROR_CODE_UNKNOWN_METACOMMAND);
                output = format!("Unknown metacommand: {}", command);
            }
        }

        Response {
            output,
            pending_input,
            error_code,
            screen,
        }
    }

    fn execute_command(&mut self, command: &str, timeout_cycles: u64) -> Response {
        // Set command
        self.machine.inject_command(command);

        let mut output = String::new();
        let mut error_code = None;

        let initial_cycle = self.cpu.cycle_count();

        // Run up to the query loop again
        if self.cpu.registers().pc() == QUERY_LOOP_ADDRESS {
            self.cpu.execute_instruction(&mut self.machine);
        }

        while self.cpu.registers().pc() != QUERY_LOOP_ADDRESS {
            self.cpu.execute_instruction(&mut self.machine);

            if self.cpu.is_halted() {
                output.push_str("HALT instruction that will never be interrupted");
                error_code = Some(ERROR_CODE_HALT);
                break;
            }

            if self.cpu.cycle_count() - initial_cycle > timeout_cycles {
                if self.trace_on_timeout {
                    self.cpu.set_trace(true);
                } else {
                    output.push_str("Timeout");
                    error_code = Some(ERROR_CODE_TIMEOUT);
                    break;
                }
            }

            let mut pc = self.cpu.registers().pc();

            // Hooks on the ROM code
            if pc < END_OF_ROM {
                match pc {
                    ROM_EMIT_CHAR_ADDRESS => {
                        output.push(ace_to_emited(self.cpu.registers().a()));
                    },

                    ROM_RAISE_ERROR_ADDRESS => {
                        let a = self.cpu.registers().a();
                        if a != ERROR_CODE_NONE { // PURGE raises a no error, so we ignore it
                            error_code = Some(self.cpu.registers().a());
                        }
                    },

                    WAIT_FOR_SYNC_HALT_ADDRESS => {
                        // Skip the HALT instruction used to slow down VLIST
                        pc += 1;
                        self.cpu.registers().set_pc(pc);
                    }

                    BEEP_LOOP_ADDRESS => {
                        // Skip the BEEP loop
                        pc += 3;
                        self.cpu.registers().set_pc(pc);
                    }

                    SPINWAIT_FOR_LISTING_ADDRESS => {
                        // Inject key press to continue LIST
                        self.machine.inject_key(0x20);
                    }
                    _ => {}
                }
            }

            // Tracing the ROM calls
            if self.trace_rom {
                match pc {
                    0x0000 => println!("ROM RESET"),
                    ROM_EMIT_CHAR_ADDRESS => println!("ROM EMIT CHAR {}-{}",
                        ace_to_screenshot(self.cpu.registers().a()), self.cpu.registers().a()),
                    //0x0010 => println!("ROM PUSH DE"),
                    //0x0018 => println!("ROM POP DE"),
                    ROM_RAISE_ERROR_ADDRESS => println!("ROM ERROR {}", self.cpu.registers().a()),
                    //0x0028 => println!("ROM FIND RAM END"),
                    0x0038 => println!("ROM VSYNC INT"),
                    _ => {}
                }
            }
        }

        output = output.replace("\r", "\n");
        let pending_input = self.machine.extract_pending_input();

        Response {
            output,
            pending_input,
            error_code,
            screen: None,
        }
    }

    pub fn save_snapshot(&self, filename: &str) -> io::Result<()> {
        let mut state = self.machine.serialize();
        state.extend(self.cpu.serialize());

        for i in 0..state.len() {
            state[i] = state[i].wrapping_sub(INITIAL_SNAPSHOT[i]);
        }

        let compressed = compress_to_vec(&state, CompressionLevel::DefaultLevel as u8);
        fs::write(filename, compressed)
    }

    pub fn load_snapshot(&mut self, filename: &str) -> io::Result<()> {
        let compressed = fs::read(filename)?;
        let mut state = match decompress_to_vec_with_limit(&compressed, 100_0000) {
            Ok(v) => v,
            Err(e) => {
                return Err(io::Error::new(io::ErrorKind::InvalidData, e.to_string()));
            }
        };

        for i in 0..state.len() {
            state[i] = state[i].wrapping_add(INITIAL_SNAPSHOT[i]);
        }
        self.load_snapshot_internal(&state)?;
        Ok(())
    }

    fn load_snapshot_internal(&mut self, state: &[u8]) -> io::Result<()> {
        self.machine.deserialize(&state[..65536]);
        self.cpu.deserialize(&state[65536..])?;
        Ok(())
    }
}
