use crate::objectmemory::{Word, UWord, ObjectMemory, OOP};

static RIGHT_MASKS: [u16; 17] = [
    0x0000,
    0x0001, 0x0003, 0x0007, 0x000F,
    0x001F, 0x003F, 0x007F, 0x00FF,
    0x01FF, 0x03FF, 0x07FF, 0x0FFF,
    0x1FFF, 0x3FFF, 0x7FFF, 0xFFFF,
];

static ALL_ONES: u16 = 0xFFFF;

#[derive(Debug, Default)]
pub struct BitBltState {
    dest_form: OOP,
    source_form: OOP,
    halftone_form: OOP,
    combination_rule: Word,
    dest_x: Word,
    dest_y: Word,
    width: Word,
    height: Word,

    clip_x: Word,
    clip_y: Word,
    clip_width: Word,
    clip_height: Word,

    source_x: Word,
    source_y: Word,

    source_bits: OOP,
    source_raster: OOP,
    dest_bits: OOP,
    dest_raster: OOP,
    halftone_bits: OOP,
    skew: Word,
    skew_mask: Word,
    mask1: Word,
    mask2: Word,
    preload: Word,
    nwords: Word,
    h_dir: Word,
    v_dir: Word,
    source_index: Word,
    source_delta: Word,
    dest_index: Word,
    dest_delta: Word,
    sx: Word,
    sy: Word,
    dx: Word,
    dy: Word,
    w: Word,
    h: Word,
}

impl super::Interpreter {
    fn clip_range(&mut self, state: &mut BitBltState) {

    }

    fn compute_masks(&mut self, state: &mut BitBltState) {

    }

    fn check_overlap(&mut self, state: &mut BitBltState) {

    }

    fn calculate_offsets(&mut self, state: &mut BitBltState) {

    }

    fn copy_loop(&mut self, state: &mut BitBltState) {

    }

    pub fn prim_copy_bits(&mut self) -> Option<()> {
        let mut state = BitBltState::default();
        self.clip_range(&mut state);
        if state.w <= 0 || state.h <= 0 {
            return Some(())
        }
        self.compute_masks(&mut state);
        self.check_overlap(&mut state);
        self.calculate_offsets(&mut state);
        self.copy_loop(&mut state);

        Some(())
    }
}