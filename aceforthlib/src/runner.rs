use std::io;

use miniz_oxide::deflate::compress_to_vec;
use miniz_oxide::deflate::CompressionLevel;
use miniz_oxide::inflate::decompress_to_vec_with_limit;

use iz80::*;

use crate::ace_machine::AceMachine;
use crate::ace_machine::END_OF_ROM;
use crate::ace_machine::FLAG_ADDRESS;
use crate::ace_machine::MAX_INPUT_BUFFER_SIZE;
use crate::characters::{ace_to_emited, ace_to_screenshot};
use crate::display::video_image;

static INITIAL_SNAPSHOT: &[u8] = include_bytes!("../../resources/initialstate.sav");

pub struct Runner {
    cpu: Cpu,
    machine: AceMachine,
    trace_rom: bool,
    trace_on_timeout: bool,
}

pub struct Response {
    pub output: String,
    pub pending_input: Option<String>,
    pub error_code: Option<u8>,
}

const ROM_EMIT_CHAR_ADDRESS: u16 = 0x0008;
const ROM_RAISE_ERROR_ADDRESS: u16 = 0x0022;
const WAIT_FOR_SYNC_HALT_ADDRESS: u16 = 0x0679;
const SPINWAIT_FOR_LISTING_ADDRESS: u16 = 0x16e1;
const BEEP_LOOP_ADDRESS: u16 = 0x0bc1;
const QUERY_LOOP_ADDRESS: u16 = 0x059b;

impl Runner {
    pub const ERROR_CODE_NONE: u8 = 255;
    pub const ERROR_CODE_QUIT: u8 = 101;
    pub const ERROR_CODE_INPUT_BUFFER_OVERFLOW: u8 = 102;
    pub const ERROR_CODE_HALT: u8 = 103;
    pub const ERROR_CODE_TIMEOUT: u8 = 104;

    pub fn new(trace_cpu: bool, trace_io: bool, trace_rom: bool) -> Runner {
        let machine = AceMachine::new(trace_io, true /* force invis */);
        let mut cpu = Cpu::new_z80();
        cpu.set_trace(trace_cpu);

        Runner {
            cpu,
            machine,
            trace_rom,
            trace_on_timeout: false,
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
        self.machine
            .poke(FLAG_ADDRESS, self.machine.peek(FLAG_ADDRESS));
    }

    pub fn prepare_with_snapshot(&mut self, snapshot: &[u8]) -> io::Result<()> {
        self.load_snapshot(snapshot)
    }

    pub fn toggle_trace_rom(&mut self) -> bool {
        self.trace_rom = !self.trace_rom;
        self.trace_rom
    }

    pub fn toggle_invis(&mut self) -> bool {
        self.machine.force_invis = !self.machine.force_invis;
        if self.machine.force_invis {
            self.machine
                .poke(FLAG_ADDRESS, self.machine.peek(FLAG_ADDRESS) | 0x10);
        } else {
            self.machine
                .poke(FLAG_ADDRESS, self.machine.peek(FLAG_ADDRESS) & !0x10);
        }
        self.machine.force_invis
    }

    pub fn get_screen_as_text(&self) -> String {
        self.machine.get_screen_as_text()
    }

    pub fn save_screen_image(&self, scanlines: bool) -> Vec<u8> {
        video_image(&self.machine, scanlines)
    }

    pub fn execute_command(&mut self, command: &str, timeout_cycles: u64) -> Response {
        if command.len() > MAX_INPUT_BUFFER_SIZE as usize {
            return Response {
                output: format!(
                    "Command line too long. The maximum supported size is {}",
                    MAX_INPUT_BUFFER_SIZE
                ),
                pending_input: None,
                error_code: Some(Runner::ERROR_CODE_INPUT_BUFFER_OVERFLOW),
            };
        }

        // Set command
        self.machine.inject_command(command);

        let mut output = String::new();
        let mut error_code = None;

        let initial_state = self.save_snapshot_internal();
        let initial_cycle = self.cpu.cycle_count();

        // Run up to the query loop again
        if self.cpu.registers().pc() == QUERY_LOOP_ADDRESS {
            self.cpu.execute_instruction(&mut self.machine);
        }

        while self.cpu.registers().pc() != QUERY_LOOP_ADDRESS {
            self.cpu.execute_instruction(&mut self.machine);

            if self.cpu.is_halted() {
                output.push_str("HALT instruction that will never be interrupted");
                error_code = Some(Runner::ERROR_CODE_HALT);
                break;
            }

            if self.cpu.cycle_count() - initial_cycle > timeout_cycles {
                if self.trace_on_timeout {
                    self.cpu.set_trace(true);
                } else {
                    output.push_str("Timeout");
                    error_code = Some(Runner::ERROR_CODE_TIMEOUT);
                    let _ = self.load_snapshot_internal(&initial_state);
                    break;
                }
            }

            let mut pc = self.cpu.registers().pc();

            // Hooks on the ROM code
            if pc < END_OF_ROM {
                match pc {
                    ROM_EMIT_CHAR_ADDRESS => {
                        output.push(ace_to_emited(self.cpu.registers().a()));
                    }

                    ROM_RAISE_ERROR_ADDRESS => {
                        let a = self.cpu.registers().a();
                        if a != Runner::ERROR_CODE_NONE {
                            // PURGE raises a no error, so we ignore it
                            error_code = Some(self.cpu.registers().a());
                        }
                    }

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
                    ROM_EMIT_CHAR_ADDRESS => println!(
                        "ROM EMIT CHAR {}-{}",
                        ace_to_screenshot(self.cpu.registers().a()),
                        self.cpu.registers().a()
                    ),
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
        }
    }

    pub fn save_snapshot(&self) -> Vec<u8> {
        let mut state = self.save_snapshot_internal();
        for i in 0..state.len() {
            state[i] = state[i].wrapping_sub(INITIAL_SNAPSHOT[i]);
        }

        compress_to_vec(&state, CompressionLevel::DefaultLevel as u8)
    }

    pub fn save_snapshot_internal(&self) -> Vec<u8> {
        let mut state = self.machine.serialize();
        state.extend(self.cpu.serialize());
        state
    }

    pub fn load_snapshot(&mut self, data: &[u8]) -> io::Result<()> {
        let mut state = match decompress_to_vec_with_limit(data, 100_0000) {
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
        self.machine.deserialize(&state[..65536 + 1]);
        self.cpu.deserialize(&state[65536 + 1..])?;
        Ok(())
    }

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
            Runner::ERROR_CODE_NONE => "No error",
            Runner::ERROR_CODE_INPUT_BUFFER_OVERFLOW => "Input buffer overflow",
            Runner::ERROR_CODE_HALT => "Halt",
            Runner::ERROR_CODE_TIMEOUT => "Timeout",
            _ => "Unknown error",
        }
    }
}
