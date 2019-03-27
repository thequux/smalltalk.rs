//! The text format is designed to be an easy translation format, independent of word size,
//! pointer format, etc. It is also designed to make patching easy
//!
//! The format is as follows:
//!
//! Tokens:
//! OOP: `@0xDEADBEEF` (object) | `$[-]0xDEADBEEF`
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
use crate::objectmemory::{ImageFormat, ObjectMemory, ObjectLayout, OOP, CLASS_COMPILED_METHOD_PTR, Object, Word};
use std::fs::File;
use std::io::{self, prelude::*, SeekFrom, BufWriter, BufReader};
use std::path::Path;
use std::fmt::{self, Display, Formatter, Arguments};
use crate::interpreter::MethodHeader;
use regex::Regex;
use sdl2::filesystem::PrefPathError::InvalidApplicationName;

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

impl InnerFormat {
    fn as_layout(self) -> ObjectLayout {
        match self {
            InnerFormat::Byte | InnerFormat::Method => ObjectLayout::Byte,
            InnerFormat::Word => ObjectLayout::Word,
            InnerFormat::Ptr => ObjectLayout::Pointer,
        }
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

fn parse_oop(text: &str) -> Option<OOP> {
    if text[0] == '$' {
        let num = if text[1..4] == "-0x" {
            &text[4..]
        } else if text[1..3] == "0x" {
            &text[3..]
        } else {
            return None;
        };

        let ival = isize::from_str_radix(num, 16)?;
        let result = OOP::from(ival as Word);
        if result.as_integer() as isize != ival {
            return None;
        }
        return Some(result)
    } else if text[0..3] == "@0x" {
        let num = usize::from_str_radix(&text[3..])?;
        let oop = OOP::pointer(num as uword);
        if oop.as_oid() != num {
            return None;
        }
        return Some(oop);
    }
    None
}

impl ImageFormat for TextFormat {
    fn load<P: AsRef<Path>>(path: P) -> io::Result<ObjectMemory> {
        let header_re = Regex::new(r"!Object: (@0x[0-9a-fA-F]+) ofClass: (@0x[0-9a-fA-F]+) format: (#byte|#word|#ptr)!").unwrap();

        let mut mem = ObjectMemory::new();

        let f = File::open(path)?;
        // We read line-by-line.
        let mut obj = None;
        let mut oop = OOP(0);

        for line in BufReader::new(f).lines() {
            let line = line?;
            if let Some(header) = header_re.captures(&line) {
                // save old object
                if let Some(obj) = obj.take() {
                    mem.objects[oop.as_oid()] = Some(obj);
                }

                let format = match header.get(3).unwrap().as_str() {
                    "#byte" => InnerFormat::Byte,
                    "#word" => InnerFormat::Word,
                    "#ptr" => InnerFormat::Ptr,
                    "#method" => InnerFormat::Method,
                };
                oop = parse_oop(header.get(1).unwrap().as_str()).unwrap();
                let klass = parse_oop(header.get(2).unwrap().as_str()).unwrap();
                obj = Some(Object{
                    class: klass,
                    layout: format.as_layout(),
                    mark: false,
                    content: vec![]
                });
            } else {
                // either bytes or words, depending
            }
        }

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
