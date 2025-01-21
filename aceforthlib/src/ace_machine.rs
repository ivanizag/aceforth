use iz80::Machine;

use crate::characters::{ace_to_emited, ace_to_screenshot, unicode_to_ace};

static ROM: &[u8] = include_bytes!("../../resources/jupiter ace.rom");

const INSCRN_ADDRESS: u16 = 0x3C1E; // Start of the input buffer
const CURSOR_ADDRESS: u16 = 0x3C20; // Cursor position
const ENDBUF_ADDRESS: u16 = 0x3C22; // End of the input buffer
const LHALF_ADDRESS:  u16 = 0x3C24; // End of the output buffer
const KEYCOD_ADDRESS: u16 = 0x3C26; // Last key pressed
const STATIN_ADDRESS: u16 = 0x3c28; // Status of the input buffer
pub const FLAG_ADDRESS:   u16 = 0x3c3e; // Flags

pub const END_OF_ROM: u16 = 0x2000;
const START_OF_SCREEN: u16 = 0x2400;
const MINIMAL_END_OF_OUTPUT_BUFFER: u16 = 0x2440;
const INITIAL_START_OF_INPUT_BUFFER: u16 = 0x26e0;
const END_OF_SCREEN: u16 = 0x2700;
pub const MAX_INPUT_BUFFER_SIZE: u16 = END_OF_SCREEN - MINIMAL_END_OF_OUTPUT_BUFFER - 2;

pub const COLUMNS: u16 = 32;
pub const ROWS: u16 = 24;

const STATIN_ENTER_MASK: u8 = 0b0010_0000;

pub struct AceMachine {
    ram: [u8; 65536],
    trace_io: bool,
    pub force_invis: bool,
}

impl AceMachine {
    pub fn new(trace_io: bool, force_invis: bool) -> AceMachine {
        AceMachine {
            ram: [0; 65536],
            trace_io,
            force_invis,
        }
    }

    pub fn inject_command(&mut self, command: &str) {
        for ch in command.chars() {
            self.inject_char(ch);
        }

        let mut statin = self.peek(STATIN_ADDRESS);
        statin |= STATIN_ENTER_MASK;
        self.poke(STATIN_ADDRESS, statin);
    }

    pub fn inject_key(&mut self, key: u8) {
        self.poke(KEYCOD_ADDRESS, key);
    }

    fn inject_char(&mut self, c: char) -> bool {
        // The char is inserted at the end of the buffer
        // Assumptions:
        //    - The cursor is at the last line of the screen
        //    - The cursor is at the end of the buffer
        let mut cursor = self.peek16(CURSOR_ADDRESS);

        if cursor == END_OF_SCREEN - 1 {
            // The cursor is at the end of the screen
            // We need to scroll the screen up if the output buffer
            // is bigger that two lines
            let mut end_of_output = self.peek16(LHALF_ADDRESS);
            if end_of_output <= MINIMAL_END_OF_OUTPUT_BUFFER {
                // We can't scroll the screen anymore
                return false;
            }

            // Scroll the screen up one line
            for i in START_OF_SCREEN..(END_OF_SCREEN - COLUMNS) {
                self.poke(i, self.peek(i+32));
            }
            for i in (END_OF_SCREEN-32)..END_OF_SCREEN {
                self.poke(i, 0x20);
            }

            let mut start_of_input = self.peek16(INSCRN_ADDRESS);
            start_of_input += 32;
            self.poke16(INSCRN_ADDRESS, start_of_input);

            end_of_output -= 32;
            self.poke16(LHALF_ADDRESS, end_of_output);

            cursor -= 32;
        }

        self.poke(cursor, unicode_to_ace(c));
        cursor += 1;
        self.poke16(CURSOR_ADDRESS, cursor);
        self.poke16(ENDBUF_ADDRESS, cursor + 1);
        true
    }

    pub fn extract_pending_input(&mut self) -> Option<String> {
        let mut command = String::new();
        let mut cursor = self.peek16(CURSOR_ADDRESS);
        cursor += 1; // Skip the cursor symbol
        let mut line = Vec::new();

        // We have to read until the enf of the screen, we can't trust endbuf when EDITing long listings
        while cursor < END_OF_SCREEN {
            let mut c = self.peek(cursor);
            if c == 0 {
                c = 0x20;
            }
            line.push(ace_to_emited(c));

            cursor += 1;
            if cursor % COLUMNS == 0 {
                let text = line.iter().collect::<String>();
                command.push_str(text.trim_end());
                line.clear();
            }
        }

        // Restore the buffers
        let buffer_start = self.peek16(INSCRN_ADDRESS);
        for i in buffer_start..END_OF_SCREEN {
            self.poke(i, 0x20);
        }
        self.poke16(INSCRN_ADDRESS, INITIAL_START_OF_INPUT_BUFFER);
        self.poke16(CURSOR_ADDRESS, INITIAL_START_OF_INPUT_BUFFER+1);
        self.poke16(ENDBUF_ADDRESS, INITIAL_START_OF_INPUT_BUFFER+2);
        self.poke16(LHALF_ADDRESS, INITIAL_START_OF_INPUT_BUFFER);
        self.poke(INITIAL_START_OF_INPUT_BUFFER, 0x00);
        self.poke(INITIAL_START_OF_INPUT_BUFFER+1, 0x97); // Cursor symbol

        if command.is_empty() {
            None
        } else {
            Some(command.to_string())
        }
    }

    pub fn get_screen_as_text(&self) -> String {
        let mut screen = String::new();
        for i in 0..(24*32) {
            let c = self.ram[i + START_OF_SCREEN as usize];
            screen.push_str(&ace_to_screenshot(c));
            if i % 32 == 31 {
                screen.push('\n');
            }
        }
        screen
    }

    pub fn get_char(&self, col: u16, row: u16) -> u8 {
        self.ram[(START_OF_SCREEN  + row * COLUMNS + col) as usize]
    }

    pub fn get_udg(&self, index: u8) -> &[u8] {
        let pos  = (0x2c00 + index as u16 * 8) as usize;
        &self.ram[pos..pos+8]
    }

    pub fn serialize(&self) -> Vec<u8> {
        self.ram.to_vec()
    }

    pub fn deserialize(&mut self, data: &[u8]) {
        self.ram.copy_from_slice(data);
    }
}

/*
Memory map (from https://k1.spdns.de/Vintage/Sinclair/80/Jupiter%20Ace/ROMs/memory.txt):
    0000-2000   8 kB ROM: ACE OS and Forth Interpreter/Compiler
    2000-2800   1 kB video RAM. actually 2 mirrored blocks 2000-2400-2800.
        2000-2300 Video memory, 768 bytes. 32 x 24 Display (used by the video system)
        2300-2400 Cassette Header Information (256 bytes)  
        2400-2700 Video memory, 768 bytes. 32 x 24 Display (used by the CPU)
            Bits 6-0 address character in the Character Definitions Table
            Bit 7 indicates inverse (black on white) text.
        2700-2800 PAD. (work space used by Forth)
    2800-3000   Character Definition Table. actually 2 mirrored blocks 2800-2C00-3000.
        2800-2C00 1 kB Character Definition Table, 128 x 8 bytes (used by video system) 
        2C00-3000 1 kB Character Definition Table, 128 x 8 bytes (used by the CPU)
    3000-4000   User Program. actually 4 mirrored blocks 3000-3400-3800-3C00-4000.
        3000-3400 Unused.
        3400-3800 Unused.
        3800-3C00 Unused.
        3C00-4000 User Program.
    4000-10000  48 kB available for RAM expansion.
*/
fn physical_address(address: u16) -> u16 {
    match address {
        0x2000..0x2300 => address + 0x400, // First copy of video RAM

        0x2800..0x2c00 => address + 0x400, // First copy of character definition table

        0x3000..0x3400 => address + 0xc00, // First copy of user program
        0x3400..0x3800 => address + 0x800, // Second copy of user program
        0x3800..0x3c00 => address + 0x400, // Third copy of user program
        _ => address
    }
}
impl Machine for AceMachine {
    fn peek(&self, address: u16) -> u8 {
        if address < ROM.len() as u16 {
            return ROM[address as usize]
        }
        let physical_address = physical_address(address);
        self.ram[physical_address as usize]
    }

    fn poke(&mut self, address: u16, value: u8) {
        let mut value = value;
        let physical_address = physical_address(address);

        if self.force_invis && physical_address == FLAG_ADDRESS {
            value |= 0x10; // Set the visible flag always false
        }

        self.ram[physical_address as usize] = value;
    }

    /*
    Ports
     */
    fn port_out(&mut self, address: u16, value: u8) {
        if self.trace_io {
            println!("OUT(0x{:04x}, 0x{:02x}): ", address, value);
        }
    }

    fn port_in(&mut self, address: u16) -> u8 {
        if self.trace_io {
            println!("IN(0x{:04x})", address);
        }

        match address {
            0xfefe => 0xff, //self.keyboard.get_keyport(0),
            0xfdfe => 0xff, //self.keyboard.get_keyport(1),
            0xfbfe => 0xff, //self.keyboard.get_keyport(2),
            0xf7fe => 0xff, //self.keyboard.get_keyport(3),
            0xeffe => 0xff, //self.keyboard.get_keyport(4),
            0xdffe => 0xff, //self.keyboard.get_keyport(5),
            0xbffe => 0xff, //self.keyboard.get_keyport(6),
            0x7ffe => 0xff, //self.keyboard.get_keyport(7),
            _ => 0xff,
        }
    }
}