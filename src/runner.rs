use iz80::*;

use super::ace_machine::AceMachine;

pub struct Runner {
    cpu: Cpu,
    machine: AceMachine,
    trace_rom: bool,
    dump_screen: bool,
}

pub struct Response {
    pub output: String,
    pub pending_input: Option<String>,
    pub error_code: Option<u8>,
    pub screen: Option<String>,
}

const METACOMMAND_PREFIX: &str = "##";

const ROM_EMIT_CHAR_ADDRESS: u16 = 0x0008;
const ROM_RAISE_ERROR_ADDRESS: u16 = 0x0022;
const WAIT_FOR_SYNC_HALT_ADDRESS: u16 = 0x0679;
const QUERY_LOOP_ADDRESS: u16 = 0x059b;

impl Runner {
    pub fn new(trace_cpu: bool, trace_io: bool, trace_rom: bool) -> Runner {
        let machine = AceMachine::new(trace_io);
        let mut cpu = Cpu::new_z80();
        cpu.set_trace(trace_cpu);

        Runner {
            cpu,
            machine,
            trace_rom,
            dump_screen: false,
        }
    }

    pub fn prepare(&mut self) {
        // Go to the query state
        while self.cpu.registers().pc() != QUERY_LOOP_ADDRESS {
            self.cpu.execute_instruction(&mut self.machine);
        }
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
        for command in commands.lines() {
            let response =  if command.starts_with(METACOMMAND_PREFIX) {
                self.execute_metacommand(&command[METACOMMAND_PREFIX.len()..])
            } else {
                self.execute_command(&command)
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

    fn execute_metacommand(&mut self, command: &str) -> Response {
        let output;
        let pending_input = None;
        let mut error_code = None;
        let mut screen = None;

        let command = &command.to_uppercase();

        if command.starts_with("DUMP") {
            output = "".to_string();
            screen = Some(self.machine.get_screen_as_text());
        } else if command.starts_with("SCREEN") {
            self.dump_screen = !self.dump_screen;
            output = format!("Screen dumping is now {}", if self.dump_screen { "enabled" } else { "disabled" });
        } else if command.starts_with("TRACE") {
            self.trace_rom = !self.trace_rom;
            output = format!("ROM tracing is now {}", if self.trace_rom { "enabled" } else { "disabled" });
        } else {
            error_code = Some(100);
            output = format!("Unknown metacommand: {}", command);
        }

        Response {
            output,
            pending_input,
            error_code,
            screen,
        }
    }

    fn execute_command(&mut self, command: &str) -> Response {

        // Set command
        self.machine.inject_command(command);

        let mut output = String::new();
        let mut error_code = None;

        // Run up to the query loop again
        if self.cpu.registers().pc() == QUERY_LOOP_ADDRESS {
            self.cpu.execute_instruction(&mut self.machine);
        }

        while self.cpu.registers().pc() != QUERY_LOOP_ADDRESS {
            self.cpu.execute_instruction(&mut self.machine);

            if self.cpu.is_halted() {
                println!("HALT instruction that will never be interrupted");
                break;
            }

            let mut pc = self.cpu.registers().pc();

            // Hooks on the ROM code
            match pc {
                ROM_EMIT_CHAR_ADDRESS => {
                    output.push(self.cpu.registers().a() as char);
                },

                ROM_RAISE_ERROR_ADDRESS => {
                    error_code = Some(self.cpu.registers().a());
                },

                WAIT_FOR_SYNC_HALT_ADDRESS => {
                    // Skip the HALT instruction used to slow down listings
                    pc += 1;
                    self.cpu.registers().set_pc(pc);
                }
                _ => {}
            }

            // Tracing the ROM calls
            if self.trace_rom {
                match pc {
                    0x0000 => println!("ROM RESET"),
                    0x0008 => println!("ROM EMIT CHAR {}-{}", self.cpu.registers().a() as char, self.cpu.registers().a()),
                    //0x0010 => println!("ROM PUSH DE"),
                    //0x0018 => println!("ROM POP DE"),
                    0x0022 => println!("ROM ERROR {}", self.cpu.registers().a()),
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
}
