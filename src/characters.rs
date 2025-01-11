/*
    Conversion of the ACE characters to Unicode and vice versa. On the ACE primitive for character
    emission all chars are sent as glyphs except '\r' which is executes a newline.

    Can be tested as follows:

        jaceforth> : CHARS 256 0 DO I EMIT LOOP ;
        jaceforth> CHARS
        CHARS  ▝▘▀▗▐▚▜ ▝▘▀▗
        ▚▜ ▝▘▀▗▐▚▜ ▝▘▀▗▐▚▜ !"#$%&'()*+,-
        ./0123456789:;<=>?@ABCDEFGHIJKLM
        NOPQRSTUVWXYZ[\]^_£abcdefghijklm
        nopqrstuvwxyz{|}~©█▙▟▄▛▌▞▖█▙▟▄▛▌
        ▞▖█▙▟▄▛▌▞▖█▙▟▄▛▌▞▖ !"#$%&'()*+,-
        ./0123456789:;<=>?@ABCDEFGHIJKLM
        NOPQRSTUVWXYZ[\]^_£abcdefghijklm
        nopqrstuvwxyz{|}~© OK  
        
    On the screenshot view, the characters from 0xc0 to 0xff are displayed inverted with ANSI codes.
*/

pub fn unicode_to_ace(c: char) -> u8 {
    match c {
        // ' ' => 0x10, // This is better mapped to the space character
        '▝' => 0x11,
        '▘' => 0x12,
        '▀' => 0x13,
        '▗' => 0x14,
        '▐' => 0x15,
        '▚' => 0x16,
        '▜' => 0x17,

        '£' => 0x60,
        '©' => 0x7f,

        '█' => 0x80,
        '▙' => 0x91,
        '▟' => 0x92,
        '▄' => 0x93,
        '▛' => 0x94,
        '▌' => 0x95,
        '▞' => 0x96,
        '▖' => 0x97,

        _ => c as u8,
    }
}

fn ace_to_symbol(code: u8) -> char {
    if (0..0x20).contains(&code) {
        return match code & 0x07 {
            0x00 => ' ',
            0x01 => '▝',
            0x02 => '▘',
            0x03 => '▀',
            0x04 => '▗',
            0x05 => '▐',
            0x06 => '▚',
            0x07 => '▜',
            _ => panic!("unreachable"),
        }
    } else if (0x80..0xa0).contains(&code) {
        return match code & 0x07 {
            0x00 => '█',
            0x01 => '▙',
            0x02 => '▟',
            0x03 => '▄',
            0x04 => '▛',
            0x05 => '▌',
            0x06 => '▞',
            0x07 => '▖',
            _ => panic!("unreachable"),
        }
    } else {
        let ascii_code = code & 0x7f;
        match ascii_code {
            0x60 => '£',
            0x7f => '©',
            _ => ascii_code as char,
        }
    }
}

pub fn ace_to_emited(code: u8) -> char {
    if code == '\r' as u8 {
        '\n'
    } else {
        ace_to_symbol(code)
    }
}

// Ansi codes are not supported on the Windows cmd out of the box
#[cfg(target_os = "windows")]
pub fn ace_to_screenshot(code: u8) -> String {
    ace_to_symbol(code).to_string()
}

#[cfg(not(target_os = "windows"))]
pub fn ace_to_screenshot(code: u8) -> String {
    if code >= 0x80 + 0x20 {
        format!("\x1b[7m{}\x1b[0m", ace_to_symbol(code))
    } else {
        ace_to_symbol(code).to_string()
    }
}