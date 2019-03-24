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
//! Byte objects contain a hex-encoded string of their contents
//!
//! Method objects start with a series of slots (as in a pointer object), followed by disassembled
//! bytecode in curly braces, one instruction per line. Symbolic labels begin with a colon

pub enum TextFormat {}
use byteorder::{ByteOrder, BigEndian, ReadBytesExt};

impl TextFormat {

}