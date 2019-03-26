//! The text format is designed to be an easy translation format, independent of word size,
//! pointer format, etc. It is also designed to make patching easy
//!
//! The format is as follows:
//!
//! Tokens:
//! OOP: `@16rDEADBEEF`
//! integers: `[-+]16rDEADBEEF`
//! format: `#byte`|`#word`|`#ptr`|`#method`
//!
//! Objects start with lines containing "!Object: <OOP> ofClass: <OOP> format: <format>!"
//!
//! Word and pointer objects are followed by one slot per line; word objects must contain only
//! integers, but pointer objects may contain OOPs
//!
//! Byte objects contain a hex-encoded string of their contents, with optional spaces between bytes
//! and at most 16 bytes per line. For word objects, spaces are only allowed between words, and the
//! words are printed in big-endian order.
//!
//! Method objects start with a series of slots (as in a pointer object), followed by disassembled
//! bytecode in curly braces, one instruction per line. Symbolic labels begin with a colon

pub enum TextFormat {}
use byteorder::{BigEndian, ByteOrder, ReadBytesExt};
use crate::objectmemory::{ImageFormat, ObjectMemory, ObjectLayout, OOP, CLASS_COMPILED_METHOD_PTR};
use std::fs::File;
use std::io::{self, prelude::*, SeekFrom, BufWriter};
use std::path::Path;
use std::fmt::{self, Display, Formatter, Arguments};
use crate::interpreter::MethodHeader;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
enum InnerFormat {
    Byte,
    Word,
    Ptr,
    Method,
}

impl Display for InnerFormat {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        let s = match self {
            InnerFormat::Byte => "#byte",
            InnerFormat::Word => "#word",
            InnerFormat::Ptr => "#ptr",
            InnerFormat::Method => "#method",
        };
        write!(f, "{}", s)
    }
}

fn obj_format(memory: &ObjectMemory, oop: OOP) -> InnerFormat {
    if memory.get_class_of(oop) == CLASS_COMPILED_METHOD_PTR {
        InnerFormat::Method
    } else {
        match memory.get_obj(oop).layout {
            ObjectLayout::Pointer => InnerFormat::Ptr,
            ObjectLayout::Word => InnerFormat::Word,
            ObjectLayout::Byte => InnerFormat::Byte,
        }
    }
}

impl ImageFormat for TextFormat {
    fn load<P: AsRef<Path>>(path: P) -> io::Result<ObjectMemory> {
        unimplemented!()
    }

    fn save<P: AsRef<Path>>(path: P, memory: &ObjectMemory) -> io::Result<()> {
        let f = File::create(path)?;
        let mut w = BufWriter::new(f);
        for i in 0..memory.objects.len() {
            let oop = OOP::pointer(i);
            if memory.objects[oop.as_oid()].is_none() {
                continue
            }

            let obj = memory.get_obj(oop);
            let format = obj_format(memory, oop);
            write!(w, "!Object: {:?} ofClass: {:?} format: {}!\n",
                oop, memory.get_class_of(oop), format,
            )?;

            match format {
                InnerFormat::Byte => {
                    for line in obj.content.chunks(16) {
                        write!(w, "\t")?;
                        for chunk in line.chunks(4) {
                            for byte in chunk {
                                write!(w, "{:02x} ", byte)?;
                            }
                            write!(w, " ")?;
                        }
                        write!(w, "\n")?;
                    }
                },
                InnerFormat::Word => {
                    for line in obj.content.chunks(16) {
                        write!(w, "\t")?;
                        for chunk in line.chunks(4) {
                            for word in chunk.chunks(2) {
                                write!(w, "{:02x}{:02x} ", word[0], word[1])?;
                            }
                            write!(w, " ")?;
                        }
                        write!(w, "\n")?;
                    }
                },
                InnerFormat::Ptr => {
                    for i in 0..memory.get_word_length_of(oop) {
                        writeln!(w, "\t{:?}", memory.get_ptr(oop, i))?;
                    }
                },
                InnerFormat::Method => {
                    let header = MethodHeader::new(memory.get_ptr(oop, 0));
                    for i in 0..header.oop_count() {
                        writeln!(w, "\t{:?}", memory.get_ptr(oop, i))?;
                    }
                    for line in obj.content[header.oop_count()*2..].chunks(16) {
                        write!(w, "\t")?;
                        for chunk in line.chunks(4) {
                            for byte in chunk {
                                write!(w, "{:02x} ", byte)?;
                            }
                            write!(w, " ")?;
                        }
                        write!(w, "\n")?;
                    }
                },
            }

        }
        Ok(())
    }
}
