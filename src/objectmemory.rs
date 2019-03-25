use byteorder::{BigEndian, ByteOrder as _};
use std::path::Path;
use std::fmt::Debug;

pub mod dist_format;
pub mod text_format;

pub type Word = i16;
pub type UWord = u16;

pub const SMALLINT_MIN: Word = -16384;
pub const SMALLINT_MAX: Word = 16383;

pub trait ImageFormat {
    fn load<P: AsRef<Path>>(path: P) -> std::io::Result<ObjectMemory>;
    fn save<P: AsRef<Path>>(path: P, memory: &ObjectMemory) -> std::io::Result<()>;
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct OOP(pub Word);

impl Default for OOP {
    fn default() -> Self {
        NIL_PTR
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
pub enum ObjectLayout {
    Byte,
    Word,
    Pointer,
}

impl ObjectLayout {
    fn field_size(self) -> usize {
        match self {
            ObjectLayout::Byte => 1,
            ObjectLayout::Word | ObjectLayout::Pointer => ::std::mem::size_of::<Word>(),
        }
    }
}

impl Debug for OOP {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        if self.is_object() {
            write!(f, "@0x{:X}", self.0 as u16)
        } else {
            write!(f, "$0x{:X}", self.as_integer())
        }
    }
}

impl OOP {
    pub fn to_pointer(mut self) -> OOP {
        OOP(self.0 & !1)
    }

    pub fn to_smallint(mut self) -> OOP {
        OOP(self.0 | 1)
    }

    pub fn as_integer(self) -> Word {
        self.try_as_integer()
            .expect("Attempted to read integer value of pointer")
    }

    pub fn try_as_integer(self) -> Option<Word> {
        if self.is_integer() {
            Some(self.0 >> 1)
        } else {
            None
        }
    }

    pub fn is_object(self) -> bool {
        self.0 & 0x1 == 0
    }

    pub fn try_from_integer(value: Word) -> Option<OOP> {
        let oop = OOP::from(value);
        if oop.as_integer() == value {
            return Some(oop);
        } else {
            return None;
        }
    }

    pub fn is_integer(self) -> bool {
        !self.is_object()
    }

    pub fn as_oid(self) -> usize {
        if !self.is_object() {
            panic!("Attempted to read pointer value of integer");
        }

        self.0 as u16 as usize >> 1
    }

    pub const fn pointer(oid: usize) -> Self {
        OOP((oid as i16) << 1)
    }

    pub const fn byte_size() -> usize {
        ::std::mem::size_of::<Word>()
    }

    pub fn as_raw(self) -> Word {
        self.0
    }
}

pub const NIL_PTR: OOP = OOP::pointer(1);
pub const FALSE_PTR: OOP = OOP::pointer(2);
pub const TRUE_PTR: OOP = OOP::pointer(3);
pub const SCHEDULER_ASSOCIATION_PTR: OOP = OOP::pointer(4);
pub const CLASS_SMALLINTEGER_PTR: OOP = OOP::pointer(6); // from classes.oops
pub const CLASS_STRING_PTR: OOP = OOP::pointer(7);
pub const CLASS_ARRAY_PTR: OOP = OOP::pointer(8);
pub const CLASS_FLOAT_PTR: OOP = OOP::pointer(10);
pub const CLASS_METHOD_CONTEXT_PTR: OOP = OOP::pointer(11);
pub const CLASS_BLOCK_CONTEXT_PTR: OOP = OOP::pointer(12);
pub const CLASS_POINT_PTR: OOP = OOP::pointer(13);
pub const CLASS_LARGE_POSITIVEINTEGER_PTR: OOP = OOP::pointer(14);
pub const CLASS_MESSAGE_PTR: OOP = OOP::pointer(16);
pub const CLASS_CHARACTER_PTR: OOP = OOP::pointer(20);
pub const DOES_NOT_UNDERSTAND_SEL: OOP = OOP::pointer(21);
pub const CANNOT_RETURN_SEL: OOP = OOP::pointer(22);
pub const MUST_BE_BOOLEAN_SEL: OOP = OOP::pointer(26);

pub const SPECIAL_SELECTORS_PTR: OOP = OOP::pointer(24);
pub const CHARACTER_TABLE_PTR: OOP = OOP::pointer(25);

impl From<i16> for OOP {
    fn from(v: i16) -> Self {
        OOP((v << 1) + 1)
    }
}

impl From<OOP> for Word {
    fn from(v: OOP) -> Word {
        v.as_integer()
    }
}

pub struct ObjectMemory {
    objects: Vec<Option<Object>>,
    ref_cnt: Vec<Word>,
}

struct Object {
    class: OOP,
    layout: ObjectLayout,
    content: Vec<u8>,
}

impl ObjectMemory {
    fn new() -> Self {
        ObjectMemory {
            objects: Vec::new(),
            ref_cnt: Vec::new(),
        }
    }

    fn get_obj(&self, oid: OOP) -> &Object {
        self.objects[oid.as_oid()].as_ref().unwrap()
    }

    fn get_obj_mut(&mut self, oid: OOP) -> &mut Object {
        self.objects[oid.as_oid()].as_mut().unwrap()
    }

    pub fn get_ptr(&self, oid: OOP, field: usize) -> OOP {
        OOP(self.get_word(oid, field))
    }

    pub fn put_ptr(&mut self, oid: OOP, field: usize, value: OOP) {
        self.put_word(oid, field, value.0);
    }

    pub fn get_word(&self, oid: OOP, field: usize) -> Word {
        let obj = self.get_obj(oid);
        let off = field * 2;
        BigEndian::read_i16(&obj.content[off..off + 2])
    }

    pub fn put_word(&mut self, oid: OOP, field: usize, value: Word) {
        let obj = self.get_obj_mut(oid);
        let off = field * 2;
        BigEndian::write_i16(&mut obj.content[off..off + 2], value)
    }

    pub fn get_float(&self, oid: OOP) -> Option<f32> {
        let obj = self.get_obj(oid);
        if obj.class != CLASS_FLOAT_PTR {
            return None;
        }
        let mut flt_u32 = 0u32;
        for i in 0..4 {
            flt_u32 = (flt_u32 << 8) + (obj.content[i] as u32);
        }
        Some(f32::from_bits(flt_u32))
    }

    pub fn put_float(&mut self, oid: OOP, value: f32) {
        let obj = self.get_obj_mut(oid);
        let mut flt_u32 = value.to_bits();
        assert_eq!(obj.class, CLASS_FLOAT_PTR);
        for i in 0..4 {
            obj.content[3 - i] = (flt_u32 >> (i * 8)) as u8;
        }
    }

    pub fn new_float(&mut self, value: f32) -> OOP {
        let obj = self.instantiate_class(CLASS_FLOAT_PTR, 2, ObjectLayout::Word);
        self.put_float(obj, value);
        obj
    }

    pub fn get_bytes(&self, oid: OOP) -> &[u8] {
        self.get_obj(oid).content.as_slice()
    }

    pub fn get_byte(&self, oid: OOP, off: usize) -> u8 {
        if !oid.is_object() {
            panic!("Attempted to read from integer");
        }
        self.get_obj(oid).content[off]
    }

    pub fn put_byte(&mut self, oid: OOP, off: usize, value: u8) {
        if !oid.is_object() {
            panic!("Attempted to write to integer");
        }
        self.get_obj_mut(oid).content[off] = value;
    }

    pub fn inc_ref(&mut self, oid: OOP) {
        if !oid.is_object() {
            return;
        }
        let oid = oid.as_oid();
        if self.ref_cnt.len() < oid {
            self.ref_cnt.resize(oid + 1, 0);
        }
        self.ref_cnt[oid] += 1;
    }

    pub fn dec_ref(&mut self, oid: OOP) {
        if !oid.is_object() {
            return;
        }
        let oid = oid.as_oid();
        if self.ref_cnt.len() < oid {
            self.ref_cnt.resize(oid + 1, 0);
        }
        self.ref_cnt[oid] -= 1;
    }

    pub fn get_class_of(&self, oid: OOP) -> OOP {
        if oid.is_integer() {
            return CLASS_SMALLINTEGER_PTR;
        }
        self.get_obj(oid).class
    }

    pub fn get_byte_length_of(&self, oid: OOP) -> usize {
        self.get_obj(oid).content.len()
    }

    pub fn get_word_length_of(&self, oid: OOP) -> usize {
        self.get_byte_length_of(oid) / 2
    }

    pub fn initial_instance_of(&self, oid: OOP) -> Option<OOP> {
        for i in 0..self.objects.len() {
            if let Some(obj) = self.objects[i].as_ref() {
                if obj.class == oid {
                    return Some(OOP::from(i as Word));
                }
            }
        }
        return None;
    }

    pub fn next_instance_of(&self, oid: OOP) -> Option<OOP> {
        let klass = self.get_class_of(oid);
        let oid = if oid.is_object() { oid.as_oid() } else { 0 };

        for i in oid..self.objects.len() {
            if let Some(obj) = self.objects[i].as_ref() {
                if obj.class == klass {
                    return Some(OOP::pointer(i));
                }
            }
        }
        None
    }

    pub fn instantiate_class(&mut self, klass: OOP, nfields: usize, layout: ObjectLayout) -> OOP {
        let byte_len = nfields * layout.field_size();

        // find an unused object
        let index = self
            .objects
            .iter()
            .enumerate()
            .find(|(index, obj)| obj.is_none())
            .map(|(index, obj)| index);

        let mut new_object = Object {
            class: klass,
            layout,
            content: vec![0; byte_len],
        };

        if layout == ObjectLayout::Pointer {
            for x in 0..nfields {
                let off = x << 1;
                BigEndian::write_i16(&mut new_object.content[off..off + 2], NIL_PTR.0);
            }
        }

        match index {
            Some(i) => {
                self.objects[i] = Some(new_object);
                OOP::pointer(i)
            }
            None => {
                self.objects.push(Some(new_object));
                self.ref_cnt.push(0);
                OOP::pointer(self.objects.len() - 1)
            }
        }
    }

    pub fn swap_pointers(&mut self, p1: OOP, p2: OOP) {
        let p1 = p1.as_oid();
        let p2 = p2.as_oid();
        self.objects.swap(p1, p2);
        self.ref_cnt.swap(p1, p2);
    }

    pub fn transfer_fields(
        &mut self,
        count: usize,
        from_obj: OOP,
        from_field: usize,
        to_obj: OOP,
        to_field: usize,
    ) {
        for i in 0..count {
            self.put_ptr(to_obj, to_field + i, self.get_ptr(from_obj, from_field + i));
            self.put_ptr(from_obj, from_field + i, NIL_PTR);
        }
    }

    pub fn oops_left(&self) -> usize {
        32767 - self.objects.iter().filter(|x| x.is_some()).count()
    }

    pub fn pad_table(&mut self) {
        while self.objects.len() < 32767 {
            self.objects.push(None);
        }
        while self.ref_cnt.len() < 32767 {
            &self.ref_cnt.push(0);
        }
    }
}
