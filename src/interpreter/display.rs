use crate::interpreter::Interpreter;
use sdl2::event::Event;
use sdl2::pixels::{Color, PixelFormatEnum};
use std::time::Instant;

pub struct StDisplay {
    last_frame: u128,

    sdl_ctx: sdl2::Sdl,
    video: sdl2::VideoSubsystem,
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
            video,
            canvas,
            event_pump,
        }
    }
}

pub fn poll_display(interp: &mut Interpreter) {
    // run event pump
    while let Some(event) = interp.display_impl.event_pump.poll_event() {
        match event {
            Event::Quit { .. } => {
                println!("Quit requested");
            }
            Event::KeyDown {
                scancode, repeat, ..
            } => {}
            Event::KeyUp { .. } => {}
            Event::MouseMotion { .. } => {}
            Event::MouseButtonDown { .. } => {}
            Event::MouseButtonUp { .. } => {}
            Event::FingerDown { .. } => {}
            Event::FingerUp { .. } => {}
            Event::FingerMotion { .. } => {}
            _ => {}
        }
    }

    // render display
    let frame_no = interp.startup_time.elapsed().as_millis() * 6 / 100;
    if interp.display_impl.last_frame < frame_no {
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

fn render_display(interp: &mut Interpreter) -> Option<()> {
    use super::bitblt::{FORM_BITS_IDX, FORM_HEIGHT_IDX, FORM_WIDTH_IDX};
    let display_form = interp.display.display;
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

    let image = sdl2::surface::Surface::from_data(
        &mut raw_owned[..],
        display_w as u32,
        display_h as u32,
        (display_w as u32 + 15) / 2,
        PixelFormatEnum::Index1MSB,
    )
    .ok()?;

    let texc = interp.display_impl.canvas.texture_creator();
    let tex = texc.create_texture_from_surface(image).ok()?;
    interp.display_impl.canvas.copy(&tex, None, None).ok()?;

    interp.display_impl.canvas.present();
    Some(())
}
