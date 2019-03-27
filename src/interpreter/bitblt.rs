use crate::objectmemory::{Word, NIL_PTR, OOP};
use crate::utils::floor_divmod;

static RIGHT_MASKS: [i16; 17] = [
    0x0000, 0x0001, 0x0003, 0x0007, 0x000F, 0x001F, 0x003F, 0x007F, 0x00FF, 0x01FF, 0x03FF, 0x07FF,
    0x0FFF, 0x1FFF, 0x3FFF, 0x7FFF, -1,
];

static ALL_ONES: i16 = -1;

#[derive(Debug, Default)]
pub struct BitBltState {
    dest_form: OOP,
    source_form: OOP,
    halftone_form: OOP,
    combination_rule: Word,
    dest_x: isize,
    dest_y: isize,
    width: isize,
    height: isize,

    clip_x: isize,
    clip_y: isize,
    clip_width: isize,
    clip_height: isize,

    source_x: isize,
    source_y: isize,
    source_wordcount: isize,

    source_bits: OOP,
    source_raster: isize,
    dest_bits: OOP,
    dest_raster: isize,
    halftone_bits: OOP,
    skew: isize,
    skew_mask: Word,
    mask1: Word,
    mask2: Word,
    preload: bool,
    nwords: isize,
    h_dir: isize,
    v_dir: isize,
    source_index: isize,
    source_delta: isize,
    dest_index: isize,
    dest_delta: isize,
    sx: isize,
    sy: isize,
    dx: isize,
    dy: isize,
    w: isize,
    h: isize,
}

const BB_DEST_FORM_IDX: usize = 0;
const BB_SOURCE_FORM_IDX: usize = 1;
const BB_HALFTONE_FORM_IDX: usize = 2;
const BB_COMBINATION_RULE_IDX: usize = 3;
const BB_DEST_X_IDX: usize = 4;
const BB_DEST_Y_IDX: usize = 5;
const BB_WIDTH_IDX: usize = 6;
const BB_HEIGHT_IDX: usize = 7;
const BB_SOURCE_X_IDX: usize = 8;
const BB_SOURCE_Y_IDX: usize = 9;
const BB_CLIP_X_IDX: usize = 10;
const BB_CLIP_Y_IDX: usize = 11;
const BB_CLIP_WIDTH_IDX: usize = 12;
const BB_CLIP_HEIGHT_IDX: usize = 13;

pub const FORM_BITS_IDX: usize = 0;
pub const FORM_WIDTH_IDX: usize = 1;
pub const FORM_HEIGHT_IDX: usize = 2;
pub const FORM_OFFSET_IDX: usize = 3;

// Useful command line:
/*
 print -l destForm sourceForm halftoneForm combinationRule destX destY width height \
          sourceX sourceY clipX clipY clipWidth clipHeight |
 perl -n -e 'chomp; my $v=$.-1; my $slotName = $_; s/[A-Z]/_\l$&/g; my $rsvar = $_; s/.*./BB_\U$&_IDX/; my $rsconst =  $_;' \
         -e 'print "$v $rsvar $rsconst $slotName\n"'
*/

// TODO: This is all very bitwidth and endianness unsafe. Must fix...
impl super::Interpreter {
    /// Clip and adjust source origin and extent appropriately
    fn clip_range(&mut self, state: &mut BitBltState) -> Option<()> {
        // first in x
        if state.dest_x >= state.clip_x {
            state.sx = state.source_x;
            state.dx = state.dest_x;
            state.w = state.width;
        } else {
            state.sx = state.source_x + (state.clip_x - state.dest_x);
            state.dx = state.clip_x;
            state.w = state.width - (state.clip_x - state.dest_x);
        }

        if state.dx + state.w > state.clip_x + state.clip_width {
            state.w -= (state.dx + state.w) - (state.clip_x + state.clip_width)
        }

        // then in y
        if state.dest_y >= state.clip_y {
            state.sy = state.source_y;
            state.dy = state.dest_y;
            state.h = state.height;
        } else {
            state.sy = state.source_y + (state.clip_y - state.dest_y);
            state.dy = state.clip_y;
            state.h = state.height - (state.clip_y - state.dest_y);
        }

        if state.dy + state.h > state.clip_y + state.clip_height {
            state.h -= (state.dy + state.h) - (state.clip_y + state.clip_height)
        }

        if state.sx < 0 {
            state.dx -= state.sx;
            state.w += state.sx;
            state.sx = 0;
        }
        if state.sy < 0 {
            state.dy -= state.sy;
            state.h += state.sy;
            state.sy = 0;
        }

        let sf_width;
        let sf_height;
        if state.source_form == NIL_PTR {
            sf_height = std::isize::MAX;
            sf_width = std::isize::MAX;
        } else {
            sf_width = self
                .memory
                .get_ptr(state.source_form, FORM_WIDTH_IDX)
                .try_as_integer()? as isize;
            sf_height = self
                .memory
                .get_ptr(state.source_form, FORM_HEIGHT_IDX)
                .try_as_integer()? as isize;
        }

        if state.sx + state.w > sf_width {
            state.w -= state.sx + state.w - sf_width;
        }
        if state.sy + state.h > sf_height {
            state.h -= state.sy + state.h - sf_height;
        }

        Some(())
    }

    /// calculate skew and edge masks
    fn compute_masks(&mut self, state: &mut BitBltState) -> Option<()> {
        state.dest_bits = self.memory.get_ptr(state.dest_form, FORM_BITS_IDX);
        state.dest_raster =
            floor_divmod(self.get_integer(state.dest_form, FORM_WIDTH_IDX)? as isize - 1, 16).0 + 1;
        if state.source_form != NIL_PTR {
            state.source_bits = self.memory.get_ptr(state.source_form, FORM_BITS_IDX);
            state.source_wordcount = self.memory.get_word_length_of(state.source_bits) as isize;
            state.source_raster =
                floor_divmod(self.get_integer(state.source_form, FORM_WIDTH_IDX)? as isize - 1, 16).0 + 1;
        }
        if state.halftone_form != NIL_PTR {
            state.halftone_bits = self.memory.get_ptr(state.halftone_form, FORM_BITS_IDX);
        }

        // How many bits source gets skewed to right
        state.skew = (state.sx - state.dx) & 0xF;
        // How many bits in first word
        let start_bits = 16 - (state.dx & 0xF);
        state.mask1 = RIGHT_MASKS[start_bits as usize] as Word;
        // how many bits in last word
        let end_bits = 15 - ((state.dx + state.w - 1) & 0xF);
        state.mask2 = !RIGHT_MASKS[end_bits as usize] as Word;

        state.skew_mask = if state.skew == 0 {
            0
        } else {
            RIGHT_MASKS[(16 - state.skew) as usize]
        };

        // determine number of words stored per line; merge masks if necessary
        if state.w < start_bits {
            state.mask1 &= state.mask2;
            state.mask2 = 0;
            state.nwords = 1;
        } else {
            state.nwords = floor_divmod(state.w - start_bits - 1, 16).0 + 2;
        }
        Some(())
    }

    fn check_overlap(&mut self, state: &mut BitBltState) -> Option<()> {
        // defaults for no overlap
        state.h_dir = 1;
        state.v_dir = 1;

        if state.source_form == state.dest_form && state.dy >= state.sy {
            if state.dy > state.sy {
                // have to start at bottom
                state.v_dir = -1;
                state.sy += state.h - 1;
                state.dy += state.h - 1;
            } else if state.dx > state.sx {
                // y's are equal, but x's are backward
                state.h_dir = -1;
                // Start at right
                state.sx += state.w - 1;
                state.dx += state.w - 1;
                // and fix up masks
                state.skew_mask = !state.skew_mask;
                let t = state.mask1;
                state.mask1 = state.mask2;
                state.mask2 = t;
            }
        }

        Some(())
    }

    fn calculate_offsets(&mut self, state: &mut BitBltState) -> Option<()> {
        state.preload =
            state.source_form != NIL_PTR && state.skew != 0 && state.skew <= state.sx & 0xF;

        if state.h_dir < 0 {
            state.preload = !state.preload
        }
        state.source_index = state.sy * state.source_raster + floor_divmod(state.sx, 16).0;
        state.dest_index = state.dy * state.dest_raster + floor_divmod(state.dx, 16).0;
        state.source_delta = state.source_raster * state.v_dir
            - state.h_dir * (state.nwords + if state.preload { 1 } else { 0 });
        state.dest_delta = state.dest_raster * state.v_dir - state.nwords * state.h_dir;

        Some(())
    }

    fn copy_loop(&mut self, state: &mut BitBltState) -> Option<()> {
        let mut prev_word;
        for _i in 1..state.h + 1 {
            let halftone_word = if state.halftone_form != NIL_PTR {
                let word = self.memory
                    .get_word(state.halftone_bits, (state.dy & 0xF) as usize);
                state.dy += state.v_dir;
                word
            } else {
                ALL_ONES
            };
            let mut skew_word = halftone_word;
            if state.preload {
                prev_word = self
                    .memory
                    .get_word(state.source_bits, state.source_index as usize);
                state.source_index += state.h_dir;
            } else {
                prev_word = 0;
            }
            let mut merge_mask = state.mask1;
            for j in 1..state.nwords + 1 {
                // horizontal inner loop
                if state.source_form != NIL_PTR {
                    // if source used
                    let this_word = if state.source_index >= state.source_wordcount {
                        0
                    } else {
                        self.memory.get_word(state.source_bits, state.source_index as usize)
                    };
                    skew_word = (prev_word & state.skew_mask) | (this_word & !state.skew_mask);
                    prev_word = this_word;
                    // 16-bit rotate
//                    skew_word = (skew_word << state.skew | skew_word >> ((16 - state.skew) & 0xF);
                    skew_word = skew_word.rotate_left(state.skew as u32);
                }
                let merge_word = Self::merge(
                    state.combination_rule,
                    skew_word & halftone_word,
                    self.memory
                        .get_word(state.dest_bits, state.dest_index as usize),
                );
                self.memory.put_word(
                    state.dest_bits,
                    state.dest_index as usize,
                    (merge_mask & merge_word)
                        | (!merge_mask
                            & self
                                .memory
                                .get_word(state.dest_bits, state.dest_index as usize)),
                );
                state.source_index += state.h_dir;
                state.dest_index += state.h_dir;
                if j == state.nwords - 1 {
                    merge_mask = state.mask2;
                } else {
                    merge_mask = ALL_ONES;
                }
            }

            state.source_index += state.source_delta;
            state.dest_index += state.dest_delta;
        }

        Some(())
    }

    fn merge(rule: Word, source: Word, dest: Word) -> Word {
        match rule {
            0 => 0,
            1 => source & dest,
            2 => source & !dest,
            3 => source,
            4 => !source & dest,
            5 => dest,
            6 => source ^ dest,
            7 => source | dest,
            8 => !source & !dest,
            9 => !source ^ dest,
            10 => !dest,
            11 => source | !dest,
            12 => !source,
            13 => !source | dest,
            14 => !source | !dest,
            15 => ALL_ONES,
            _ => 0,
        }
    }

    fn load_state(&mut self, oop: OOP, state: &mut BitBltState) -> Option<()> {
        state.dest_form = self.memory.get_ptr(oop, BB_DEST_FORM_IDX);
        state.source_form = self.memory.get_ptr(oop, BB_SOURCE_FORM_IDX);
        state.halftone_form = self.memory.get_ptr(oop, BB_HALFTONE_FORM_IDX);
        state.combination_rule = self
            .memory
            .get_ptr(oop, BB_COMBINATION_RULE_IDX)
            .try_as_integer()?;
        state.dest_x = self.memory.get_ptr(oop, BB_DEST_X_IDX).try_as_integer()? as isize;
        state.dest_y = self.memory.get_ptr(oop, BB_DEST_Y_IDX).try_as_integer()? as isize;
        state.width = self.memory.get_ptr(oop, BB_WIDTH_IDX).try_as_integer()? as isize;
        state.height = self.memory.get_ptr(oop, BB_HEIGHT_IDX).try_as_integer()? as isize;
        state.source_x = self.memory.get_ptr(oop, BB_SOURCE_X_IDX).try_as_integer()? as isize;
        state.source_y = self.memory.get_ptr(oop, BB_SOURCE_Y_IDX).try_as_integer()? as isize;
        state.clip_x = self.memory.get_ptr(oop, BB_CLIP_X_IDX).try_as_integer()? as isize;
        state.clip_y = self.memory.get_ptr(oop, BB_CLIP_Y_IDX).try_as_integer()? as isize;
        state.clip_width = self
            .memory
            .get_ptr(oop, BB_CLIP_WIDTH_IDX)
            .try_as_integer()? as isize;
        state.clip_height = self
            .memory
            .get_ptr(oop, BB_CLIP_HEIGHT_IDX)
            .try_as_integer()? as isize;
        Some(())
    }

    pub fn prim_copy_bits(&mut self) -> Option<()> {
        let mut state = BitBltState::default();
        let rcvr = self.stack_top();
        self.load_state(rcvr, &mut state)?;
        self.clip_range(&mut state)?;
        if state.w <= 0 || state.h <= 0 {
            return Some(());
        }
        self.compute_masks(&mut state)?;
        self.check_overlap(&mut state)?;
        self.calculate_offsets(&mut state)?;
        self.copy_loop(&mut state)?;

        Some(())
    }
}
