use std::path::Path;
use std::io::{self, prelude::*, SeekFrom};
use std::fs::File;
use crate::objectmemory::{ObjectMemory, OOP, ObjectLayout};
use byteorder::{ByteOrder, BigEndian, ReadBytesExt};
use crate::interpreter::InstanceSpecification;

pub enum DistFormat{}

struct ObjTblEntry {
    flags: u16,
    location: u16,
}

impl ObjTblEntry {
    fn refcnt(&self) -> usize {
        (self.flags >> 8) as usize
    }

    fn odd_size(&self) -> bool {
        self.flags & 0x80 != 0
    }

    fn has_ptrs(&self) -> bool {
        self.flags & 0x40 != 0
    }

    fn is_free(&self) -> bool {
        self.flags & 0x20 != 0
    }

    fn segment(&self) -> u8 {
        self.flags as u8 & 0xf
    }
}

impl super::ImageFormat for DistFormat {
    fn load<P: AsRef<Path>>(path: P) -> io::Result<ObjectMemory> {
        let mut f = File::open(path)?;

        let obj_space_len = f.read_u32::<BigEndian>()? as usize;
        let obj_table_len = f.read_u32::<BigEndian>()? as usize;
        let obj_space_off = 512;
        let obj_table_off = (obj_space_len * 2 + 511) / 512 * 512 + obj_space_off;

        // read the object table...
        f.seek(SeekFrom::Start(obj_table_off as u64))?;
        let mut raw_objtbl = Vec::with_capacity(obj_table_len / 2);
        for i in 0..obj_table_len / 2 {
            let flags = f.read_u16::<BigEndian>()?;
            let location = f.read_u16::<BigEndian>()?;
            raw_objtbl.push(ObjTblEntry { flags, location });
        }

        let mut memory = super::ObjectMemory::new();

        // Read objects
        for (i, entry) in raw_objtbl.iter().enumerate() {
            if entry.is_free() {
                memory.ref_cnt.push(0);
                memory.objects.push(None);
            } else {
                f.seek(SeekFrom::Start(entry.location as u64 * 2 + 512))?;
                let class_oop = OOP(f.read_i16::<BigEndian>()?);
                let len = f.read_u16::<BigEndian>()?;
                let real_len = len as usize * 2 + if entry.odd_size() { 1 } else { 0 };

                let mut content = Vec::new();
                content.resize(real_len, 0);
                f.read_exact(&mut content[..])?;

                let obj = super::Object {
                    class: class_oop,
                    layout: ObjectLayout::Byte,
                    content,
                };
                memory.objects.push(Some(obj))
            }
        }

        // Patch up object formats...
        for i in 0..memory.objects.len() {
            if memory.objects[i].is_none() {
                continue;
            }
            let class = memory.objects[i].as_ref().unwrap().class;
            let ispec: InstanceSpecification = memory.get_ptr(class, 2).into();
            memory.objects[i].as_mut().unwrap().layout = if ispec.is_pointers() {
                ObjectLayout::Word
            } else if ispec.is_words() {
                ObjectLayout::Pointer
            } else {
                ObjectLayout::Byte
            }
        }

        Ok(memory)
    }

    fn save<P: AsRef<Path>>(path: P, memory: &ObjectMemory) -> io::Result<()> {
        unimplemented!()
    }
}