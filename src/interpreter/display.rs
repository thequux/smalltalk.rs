use crate::interpreter::{Interpreter, StEvent};
use sdl2::event::{Event, WindowEvent};
use sdl2::pixels::{Color, PixelFormatEnum};
use crate::objectmemory::{UWord, NIL_PTR};
use sdl2::rect::Rect;
use sdl2::mouse::MouseButton;
use sdl2::render::BlendMode;

pub struct StDisplay {
    last_frame: u128,

    sdl_ctx: sdl2::Sdl,
    canvas: sdl2::render::WindowCanvas,
    event_pump: sdl2::EventPump,
}

impl StDisplay {
    pub fn new() -> Self {
        let ctx = sdl2::init().expect("SDL2 init failed");
        let video = ctx.video().unwrap();
        let window = video
            .window("rsvm", 640, 480)
            .position_centered()
            .build()
            .unwrap();

        let mut canvas = window.into_canvas().build().unwrap();
        canvas.set_draw_color(Color::RGB(0x00, 0x88, 0xFF));
        canvas.clear();
        canvas.present();

        let event_pump = ctx.event_pump().unwrap();
        Self {
            sdl_ctx: ctx,
            last_frame: 0,
            canvas,
            event_pump,
        }
    }
}

pub fn notice_new_display(interp: &mut Interpreter) -> Option<()> {
    use super::bitblt::{FORM_BITS_IDX, FORM_HEIGHT_IDX, FORM_WIDTH_IDX};
    let display_form = interp.display.display;
    let display_w = interp
        .memory
        .get_ptr(display_form, FORM_WIDTH_IDX)
        .try_as_integer()?;
    let display_h = interp
        .memory
        .get_ptr(display_form, FORM_HEIGHT_IDX)
        .try_as_integer()?;
    interp.display_impl.canvas.window_mut().set_size(display_w as u32 * DISPLAY_SCALE, display_h as u32 * DISPLAY_SCALE).unwrap();
    Some(())
}


fn mouse_to_st(btn: MouseButton) -> Option<UWord> {
    match btn {
        MouseButton::Left => Some(130),
        MouseButton::Middle => Some(129),
        MouseButton::Right => Some(128),
        _ => None,
    }
}

fn key_to_st(key: sdl2::keyboard::Scancode) -> Option<char> {
    use sdl2::keyboard::Scancode;
    match key {
        Scancode::Backspace => Some(8 as char),
        Scancode::Tab => Some(9 as char),
        Scancode::Return => Some(13 as char),
        Scancode::Escape => Some(27 as char),
        Scancode::Space => Some(' '),
        Scancode::Comma => Some(','),
        Scancode::Minus => Some('-'),
        Scancode::Period => Some('.'),
        Scancode::Slash => Some('/'),
        Scancode::Num0 => Some('0'),
        Scancode::Num1 => Some('1'),
        Scancode::Num2 => Some('2'),
        Scancode::Num3 => Some('3'),
        Scancode::Num4 => Some('4'),
        Scancode::Num5 => Some('5'),
        Scancode::Num6 => Some('6'),
        Scancode::Num7 => Some('7'),
        Scancode::Num8 => Some('8'),
        Scancode::Num9 => Some('9'),
        Scancode::Semicolon => Some(';'),
        Scancode::Equals => Some('='),
        Scancode::LeftBracket => Some('['),
        Scancode::Backslash => Some('\\'),
        Scancode::RightBracket => Some(']'),
        Scancode::A => Some('a'),
        Scancode::B => Some('b'),
        Scancode::C => Some('c'),
        Scancode::D => Some('d'),
        Scancode::E => Some('e'),
        Scancode::F => Some('f'),
        Scancode::G => Some('g'),
        Scancode::H => Some('h'),
        Scancode::I => Some('i'),
        Scancode::J => Some('j'),
        Scancode::K => Some('k'),
        Scancode::L => Some('l'),
        Scancode::M => Some('m'),
        Scancode::N => Some('n'),
        Scancode::O => Some('o'),
        Scancode::P => Some('p'),
        Scancode::Q => Some('q'),
        Scancode::R => Some('r'),
        Scancode::S => Some('s'),
        Scancode::T => Some('t'),
        Scancode::U => Some('u'),
        Scancode::V => Some('v'),
        Scancode::W => Some('w'),
        Scancode::X => Some('x'),
        Scancode::Y => Some('y'),
        Scancode::Z => Some('z'),
        Scancode::Delete => Some(127 as char),
        Scancode::CapsLock => Some(139 as char),
        Scancode::Right => Some(131 as char),
        Scancode::Up => Some(132 as char),
        Scancode::Down => Some(133 as char),
        Scancode::Left => Some(134 as char), // 135 in this set
//        Scancode::NumLockClear => Some(''),
        Scancode::KpDivide => Some('/'),
        Scancode::KpMultiply => Some('*'),
        Scancode::KpMinus => Some('-'),
        Scancode::KpPlus => Some('+'),
        Scancode::KpEnter => Some(10 as char),
        Scancode::Kp1 => Some('1'),
        Scancode::Kp2 => Some('2'),
        Scancode::Kp3 => Some('3'),
        Scancode::Kp4 => Some('4'),
        Scancode::Kp5 => Some('5'),
        Scancode::Kp6 => Some('6'),
        Scancode::Kp7 => Some('7'),
        Scancode::Kp8 => Some('8'),
        Scancode::Kp9 => Some('9'),
        Scancode::Kp0 => Some('0'),
        Scancode::KpPeriod => Some('.'),
        Scancode::KpEquals => Some('='),
        Scancode::KpComma => Some(','),
        Scancode::KpLeftParen => Some('('),
        Scancode::KpRightParen => Some(')'),
        Scancode::KpLeftBrace => Some('['),
        Scancode::KpRightBrace => Some(']'),
        Scancode::KpTab => Some(9 as char),
        Scancode::KpBackspace => Some(8 as char),
        Scancode::KpA => Some('a'),
        Scancode::KpB => Some('b'),
        Scancode::KpC => Some('c'),
        Scancode::KpD => Some('d'),
        Scancode::KpE => Some('e'),
        Scancode::KpF => Some('f'),
        Scancode::KpPercent => Some('%'),
        Scancode::KpLess => Some('<'),
        Scancode::KpGreater => Some('>'),
        Scancode::KpAmpersand => Some('&'),
        Scancode::KpVerticalBar => Some('|'),
        Scancode::KpColon => Some(':'),
        Scancode::KpHash => Some('#'),
        Scancode::KpSpace => Some(' '),
        Scancode::KpAt => Some('@'),
        Scancode::KpExclam => Some('!'),
        Scancode::LCtrl => Some(138 as char),
        Scancode::LShift => Some(136 as char),
        Scancode::LAlt => Some(27 as char),
        Scancode::RCtrl => Some(138 as char),
        Scancode::RShift => Some(137 as char),
        Scancode::RAlt => Some(27 as char),
        _ => None,
    }
}

pub fn poll_display(interp: &mut Interpreter) {
    // run event pump
    while let Some(event) = interp.display_impl.event_pump.poll_event() {
        match event {
            Event::Quit { .. } => {
                ::std::process::exit(0);
            }
            Event::KeyDown {
                scancode, repeat, ..
            } => {
                if repeat { continue };
                if let Some(chr) = scancode.and_then(key_to_st) {
                    interp.push_event(StEvent::Bistate(chr as UWord, true));
                }
            }
            Event::KeyUp { scancode, .. } => {
                if let Some(chr) = scancode.and_then(key_to_st) {
                    interp.push_event(StEvent::Bistate(chr as UWord, false));
                }
            }
            Event::MouseMotion { x, y, .. } => {
                interp.push_event(StEvent::PointerPos((x / DISPLAY_SCALE as i32) as UWord, (y / DISPLAY_SCALE as i32) as UWord));
            }
            Event::MouseButtonDown { mouse_btn, .. } => {
                if let Some(btn) = mouse_to_st(mouse_btn) {
                    interp.push_event(StEvent::Bistate(btn, true));
                }
            }
            Event::MouseButtonUp { mouse_btn, .. } => {
                if let Some(btn) = mouse_to_st(mouse_btn) {
                    interp.push_event(StEvent::Bistate(btn, false));
                }
            }
            Event::FingerDown { .. } => {}
            Event::FingerUp { .. } => {}
            Event::FingerMotion { .. } => {},
            Event::Window { win_event: WindowEvent::Enter, .. } => {
                interp.display_impl.sdl_ctx.mouse().show_cursor(false);
            }
            Event::Window { win_event: WindowEvent::Leave, .. } => {
                interp.display_impl.sdl_ctx.mouse().show_cursor(true);
            }
            _ => {}
        }
    }

    // render display
    let frame_no = interp.startup_time.elapsed().as_millis() * 6 / 100;
    if interp.display_impl.last_frame < frame_no {
//        println!("Triggered frame");
        interp.display_impl.last_frame = frame_no;
        if render_display(interp).is_none() {
            let canvas = &mut interp.display_impl.canvas;
            canvas.set_draw_color(Color::RGB(255, 0, 0));
            canvas.clear();
            canvas.present();
            // render error
        };
    }
}

const DISPLAY_SCALE: u32 = 2;

fn render_display(interp: &mut Interpreter) -> Option<()> {
    use super::bitblt::{FORM_BITS_IDX, FORM_HEIGHT_IDX, FORM_WIDTH_IDX};


    let display_form = interp.display.display;
    if display_form == NIL_PTR {
        return None;
    }

    let mut canvas = &mut interp.display_impl.canvas;
    let display_bits = interp.memory.get_ptr(display_form, FORM_BITS_IDX);
    let display_raw = interp.memory.get_bytes(display_bits);
    let display_w = interp
        .memory
        .get_ptr(display_form, FORM_WIDTH_IDX)
        .try_as_integer()?;
    let display_h = interp
        .memory
        .get_ptr(display_form, FORM_HEIGHT_IDX)
        .try_as_integer()?;

    let mut raw_owned = display_raw.to_vec();

//    println!("DISP size: {}x{}", display_w, display_h);

    if false {
        return Some(())
    }
    let image = sdl2::surface::Surface::from_data(
        &mut raw_owned[..],
        display_w as u32,
        display_h as u32,
        (display_w as u32 + 15) / 16 * 2,
        PixelFormatEnum::Index1MSB,
    )
    .ok()?;

    let texc = canvas.texture_creator();
    let tex = texc.create_texture_from_surface(image).ok()?;
    canvas.copy(&tex, None, None).ok()?;
    
    if interp.display.cursor != NIL_PTR {
        let cursor_form = interp.display.cursor;
        if cursor_form == NIL_PTR {
            return None;
        }
        let cursor_bits = interp.memory.get_ptr(cursor_form, FORM_BITS_IDX);
        let cursor_raw = interp.memory.get_bytes(cursor_bits);
        let cursor_w = interp
            .memory
            .get_ptr(cursor_form, FORM_WIDTH_IDX)
            .try_as_integer()?;
        let cursor_h = interp
            .memory
            .get_ptr(cursor_form, FORM_HEIGHT_IDX)
            .try_as_integer()?;

        let mut raw_owned = cursor_raw.to_vec();
        let image = sdl2::surface::Surface::from_data(
            &mut raw_owned[..],
            cursor_w as u32,
            cursor_h as u32,
            (cursor_w as u32 + 15) / 16 * 2,
            PixelFormatEnum::Index1MSB,
        )
            .ok()?;
        let tex = texc.create_texture_from_surface(image).ok()?;
        let cursor_pos = interp.display.cursor_location.unwrap_or(interp.display.mouse_location);
        let dest_rect = Rect::new(
            cursor_pos.0 as i32 * DISPLAY_SCALE as i32,
            cursor_pos.1 as i32 * DISPLAY_SCALE as i32,
            cursor_w as u32 * DISPLAY_SCALE,
            cursor_h as u32 * DISPLAY_SCALE,
        );
        canvas.set_blend_mode(BlendMode::Add);
        canvas.set_draw_color(Color::RGB(0,128,255));
        canvas.copy(&tex, None, dest_rect).ok()?;

    }

    canvas.present();
    Some(())
}
