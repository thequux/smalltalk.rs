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
use crate::objectmemory::{ImageFormat, ObjectMemory, ObjectLayout, OOP, CLASS_COMPILED_METHOD_PTR, Object, Word, UWord};
use std::fs::File;
use std::io::{self, prelude::*, SeekFrom, BufWriter, BufReader};
use std::path::Path;
use std::fmt::{self, Display, Formatter, Arguments};
use crate::interpreter::MethodHeader;
use regex::Regex;
use sdl2::filesystem::PrefPathError::InvalidApplicationName;
use failure::{Error, Fail};

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
    if text.as_bytes()[0] == b'$' {
        let (num, neg) = if &text[1..4] == "-0x" {
            (&text[4..], true)
        } else if &text[1..3] == "0x" {
            (&text[3..], false)
        } else {
            return None;
        };

        let ival = isize::from_str_radix(num, 16).ok()? * if neg { -1 } else { 1 };
        let result = OOP::from(ival as Word);
        if result.as_integer() as isize != ival {
            return None;
        }
        return Some(result)
    } else if &text[0..3] == "@0x" {
        let num = usize::from_str_radix(&text[3..], 16).ok()?;
        let oop = OOP::pointer(num as UWord as usize);
        if oop.as_oid() != num {
            return None;
        }
        return Some(oop);
    }
    None
}

impl ImageFormat for TextFormat {
    fn load<P: AsRef<Path>>(path: P) -> Result<ObjectMemory, failure::Error> {
        let header_re = Regex::new(r"!Object: (@0x[0-9a-fA-F]+) ofClass: (@0x[0-9a-fA-F]+) format: (#byte|#word|#ptr|#method)!").unwrap();

        let mut mem = ObjectMemory::new();
        mem.pad_table(); // make sure all target objects exist

        let f = File::open(path)?;
        // We read line-by-line.
        let mut obj = None;
        let mut oop = OOP(0);

        'line: for (line_no, line) in BufReader::new(f).lines().enumerate() {
            let line = line?;
            (||{
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
                        _ => panic!("Invalid object type"),
                    };
                    oop = parse_oop(header.get(1).unwrap().as_str()).ok_or_else(||failure::err_msg("Invalid OOP"))?;
                    let klass = parse_oop(header.get(2).unwrap().as_str()).ok_or_else(||failure::err_msg("Invalid OOP"))?;
                    obj = Some(Object{
                        class: klass,
                        layout: format.as_layout(),
                        mark: false,
                        content: vec![]
                    });
                } else {
                    // either bytes or words, depending on the object format
                    // we cheat. Because we know that words are big-endian and always presented
                    // in their full glory, we can treat everything as bytes.
                    // Oops are broken into big-endian form and then shoved in directly
                    let mut line = line.as_str();
                    loop {
                        line = line.trim();
                        let start = if let Some(c1) = line.chars().next() {
                            c1
                        } else {
                            break
                        };

                        match start {
                            '0'...'9'|'A'...'F'|'a'...'f' => {
                                // hex line, either bytes or words. We could look it up but see above
                                // re: cheating
                                let (next_word, rest) = line.split_at(2);
                                line = rest;
                                let byte = u8::from_str_radix(next_word, 16)?;
                                obj.as_mut().unwrap().content.push(byte);
                            },
                            '@' | '$' => {
                                let (next_word, rest) = if let Some(n) = line.find(char::is_whitespace) {
                                    (&line[0..n], &line[n..])
                                } else {
                                    (line, "")
                                };
                                line = rest;
                                let oop = parse_oop(next_word).ok_or_else(||failure::err_msg("Invalid OOP"))?;
                                obj.as_mut().ok_or_else(|| failure::err_msg("No active object"))?
                                    .content.extend_from_slice(&oop.0.to_be_bytes()[..]);
                            },
                            _ => {
                                panic!("Invalid char {:?} on line {}", start, line_no);
                            }
                        }
                    }
                }
                Ok(())
            })().map_err(|err: failure::Error| {
                println!("Error on line {}", line_no);
                err.context(format!("on line {}", line_no))
            })?;
        }

        if let Some(obj) = obj.take() {
            mem.objects[oop.as_oid()] = Some(obj);
        }

        Ok(mem)
    }

    fn save<P: AsRef<Path>>(path: P, memory: &ObjectMemory) -> Result<(), failure::Error> {
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
