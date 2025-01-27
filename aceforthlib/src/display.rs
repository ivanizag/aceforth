use std::io::Cursor;

use image::ImageBuffer;
use image::Rgb;

use crate::ace_machine::AceMachine;
use crate::ace_machine::COLUMNS;
use crate::ace_machine::ROWS;

const SCREEN_WIDTH: u32 = 256;
const SCREEN_HEIGHT: u32 = 192;

pub fn video_image(ace_machine: &AceMachine) -> Vec<u8> {
    let mut fb = ImageBuffer::new(SCREEN_WIDTH, SCREEN_HEIGHT);

    for x in 0..COLUMNS {
        for y in 0..ROWS {
            let ch = ace_machine.get_char(x, y);
            let inverse = ch & 0x80 != 0;

            let udg = ace_machine.get_udg(ch & 0x7f);
            #[allow(clippy::needless_range_loop)]
            for j in 0..8 {
                let mut line = udg[j];
                if inverse {
                    line = !line;
                }
                for i in (0..8).rev() {
                    let pixel = if line & 1 == 0 { 0_u8 } else { 255_u8 };
                    fb.put_pixel(
                        (8 * x + i) as u32,
                        8 * y as u32 + j as u32,
                        Rgb([pixel, pixel, pixel]),
                    );
                    line >>= 1;
                }
            }
        }
    }

    let mut buf = Vec::new();
    fb.write_to(&mut Cursor::new(&mut buf), image::ImageFormat::Png)
        .unwrap();
    buf
}
