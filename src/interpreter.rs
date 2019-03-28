use crate::interpreter::HeaderFlag::PrimSelf;
use crate::objectmemory::{ObjectLayout, ObjectMemory, UWord, Word, CANNOT_RETURN_SEL, CHARACTER_TABLE_PTR, CLASS_ARRAY_PTR, CLASS_BLOCK_CONTEXT_PTR, CLASS_CHARACTER_PTR, CLASS_LARGE_POSITIVEINTEGER_PTR, CLASS_MESSAGE_PTR, CLASS_METHOD_CONTEXT_PTR, CLASS_POINT_PTR, CLASS_STRING_PTR, DOES_NOT_UNDERSTAND_SEL, FALSE_PTR, MUST_BE_BOOLEAN_SEL, NIL_PTR, OOP, SCHEDULER_ASSOCIATION_PTR, SPECIAL_SELECTORS_PTR, TRUE_PTR, CLASS_FLOAT_PTR};
use std::collections::{VecDeque, HashSet, HashMap};
use std::borrow::Cow;
use std::cell::{RefCell, Cell};
use std::rc::Rc;
use crate::interpreter::gc_support::HeldOops;
use std::fs::File;
use std::io::BufRead;
use crate::utils::floor_divmod;
use std::time::{Instant, SystemTime, UNIX_EPOCH};

mod bitblt;
mod display;
mod startup;

const DBG_INSN: bool = false;
const DBG_CALL: bool = false;
const DBG_LOOKUP: bool = false;
const DBG_MEM: bool = false;

mod gc_support;

pub struct Interpreter {
    memory: ObjectMemory,
    active_context: OOP,
    home_context: OOP,
    method: OOP,
    receiver: OOP,

    // These should really be unsigned
    ip: usize,
    sp: usize,

    cycle: usize,
    call_depth: usize,

    // benchmark
    bmark_cycles: usize,
    bmark_lastprint: u64,

    // Message lookup process
    message_selector: OOP,
    argument_count: usize,
    new_method: OOP,
    primitive_index: usize,

    method_cache: [MethodCacheEntry; 256],

    // process scheduler
    new_process: Option<OOP>,
    semaphore_list: Vec<OOP>,

    // display
    display: DisplayState,
    display_impl: self::display::StDisplay,

    // io
    startup_time: ::std::time::Instant,
    timer_sem: Option<OOP>,
    timer_when: u32,

    // GC support
    held_objects: Rc<RefCell<Vec<Vec<OOP>>>>,
    dbg_alloc: Cell<bool>,
}

// Method constants
const HEADER_INDEX: usize = 0;
const LITERAL_START: usize = 1;

// Association constants
const VALUE_INDEX: usize = 1;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
pub struct MethodHeader(usize);

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
pub enum HeaderFlag {
    // no primitive, 0-4 arguments (in value)
    Normal(usize),
    PrimSelf,
    PrimReturn,
    HeaderExt,
}

impl MethodHeader {
    pub fn new(oop: OOP) -> Self {
        MethodHeader(oop.as_integer() as usize)
    }

    pub fn temporary_count(self) -> usize {
        (self.0 >> 7) & 0x1F
    }

    pub fn large_context_flag(self) -> bool {
        self.0 & 0x40 != 0
    }

    pub fn literal_count(self) -> usize {
        self.0 & 0x3F
    }

    pub fn oop_count(self) -> usize {
        self.literal_count() + LITERAL_START
    }

    pub fn initial_ip(self) -> usize {
        // This might be wrong, as we are zero-indexed
        self.oop_count() * OOP::byte_size() + 1
    }

    pub fn flag_value(self) -> HeaderFlag {
        use self::HeaderFlag::*;
        match (self.0 >> 12) & 0x7 {
            count @ (0...4) => HeaderFlag::Normal(count),
            5 => PrimSelf,
            6 => PrimReturn,
            7 => HeaderExt,
            _ => unreachable!("Should be exhaustive"),
        }
    }

    pub fn field_index(self) -> usize {
        self.temporary_count()
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
struct HeaderExt(usize);

impl HeaderExt {
    fn new(oop: OOP) -> HeaderExt {
        HeaderExt(oop.as_integer() as usize)
    }

    fn primitive_index(self) -> usize {
        self.0 & 0xFF
    }

    fn argument_count(self) -> usize {
        (self.0 >> 8) & 0x1F
    }
}

impl Interpreter {
    // compiled methods
    fn method_header_of(&self, method: OOP) -> MethodHeader {
        MethodHeader::new(self.memory.get_ptr(method, 0))
    }

    pub fn method_literal_of(&self, method: OOP, offset: usize) -> OOP {
        self.memory.get_ptr(method, LITERAL_START + offset)
    }

    // method headers
    #[allow(unused)]
    fn header_extension(&self, method: OOP) -> HeaderExt {
        self.header_extension_fast(method, self.method_header_of(method))
    }

    fn header_extension_fast(&self, method: OOP, header: MethodHeader) -> HeaderExt {
        let oop = self.method_literal_of(method, header.literal_count() - 2);
        HeaderExt::new(oop)
    }

    pub fn argument_count(&self, method: OOP) -> usize {
        let header = self.method_header_of(method);
        match header.flag_value() {
            HeaderFlag::Normal(arg_count) => arg_count,
            HeaderFlag::HeaderExt => self.header_extension_fast(method, header).argument_count(),
            HeaderFlag::PrimReturn | HeaderFlag::PrimSelf => 0,
        }
    }

    pub fn primitive_index(&self, method: OOP) -> usize {
        self.primitive_index_fast(method, self.method_header_of(method))
    }

    fn primitive_index_fast(&self, method: OOP, header: MethodHeader) -> usize {
        if header.flag_value() == HeaderFlag::HeaderExt {
            self.header_extension_fast(method, header).primitive_index()
        } else {
            0
        }
    }

    fn method_class(&self, method: OOP) -> OOP {
        self.method_class_fast(method, self.method_header_of(method))
    }

    fn method_class_fast(&self, method: OOP, header: MethodHeader) -> OOP {
        let klass_assoc = self.method_literal_of(method, header.literal_count() - 1);
        self.memory.get_ptr(klass_assoc, VALUE_INDEX)
    }
}

// Context stuff

const CTX_SENDER_INDEX: usize = 0;
const CTX_IP_INDEX: usize = 1;
const CTX_SP_INDEX: usize = 2;
const CTX_METHOD_INDEX: usize = 3;
const CTX_RECEIVER_INDEX: usize = 5;
const CTX_TEMPFRAME_START_INDEX: usize = 6;

const CTX_CALLER_INDEX: usize = 0;
const CTX_BLOCK_ARG_COUNT_INDEX: usize = 3;
const CTX_INITIAL_IP_INDEX: usize = 4;
const CTX_HOME_INDEX: usize = 5;

/// Context methods
impl Interpreter {
    pub fn context_get_ip(&self, context: OOP) -> Word {
        self.memory.get_ptr(context, CTX_IP_INDEX).as_integer()
    }

    pub fn context_put_ip(&mut self, context: OOP, new_val: Word) {
        self.memory
            .put_ptr(context, CTX_IP_INDEX, OOP::from(new_val))
    }

    pub fn context_get_sp(&self, context: OOP) -> Word {
        self.memory.get_ptr(context, CTX_SP_INDEX).as_integer()
    }

    pub fn context_put_sp(&mut self, context: OOP, new_val: Word) {
        self.memory
            .put_ptr(context, CTX_SP_INDEX, OOP::from(new_val))
    }

    pub fn block_argument_count(&mut self, context: OOP) -> usize {
        self.memory.get_ptr(context, CTX_BLOCK_ARG_COUNT_INDEX).as_integer() as usize
    }

    pub fn is_block_ctx(&self, context: OOP) -> bool {
        !self.memory.get_ptr(context, CTX_METHOD_INDEX).is_object()
    }

    pub fn load_ctx(&mut self) {
        if self.is_block_ctx(self.active_context) {
            self.home_context = self.memory.get_ptr(self.active_context, CTX_HOME_INDEX);
        } else {
            self.home_context = self.active_context;
        }

        self.receiver = self.memory.get_ptr(self.home_context, CTX_RECEIVER_INDEX);
        self.method = self.memory.get_ptr(self.home_context, CTX_METHOD_INDEX);
        self.ip = self.context_get_ip(self.active_context) as UWord as usize - 1;
        self.sp = self.context_get_sp(self.active_context) as UWord as usize
            + CTX_TEMPFRAME_START_INDEX
            - 1;

        let mut sender = self.active_context;
        self.call_depth = 0;
        while sender != NIL_PTR {
            self.call_depth += 1;
            sender = self.memory.get_ptr(sender, CTX_SENDER_INDEX);
        }
    }

    pub fn save_ctx(&mut self) {
        self.context_put_ip(self.active_context, (self.ip + 1) as Word);
        self.context_put_sp(
            self.active_context,
            (self.sp as Word + 1 - CTX_TEMPFRAME_START_INDEX as Word),
        );
    }

    pub fn push(&mut self, oop: OOP) {
        self.sp += 1;
        if DBG_MEM {
            println!("STCK {:?}_{} <- {}", self.active_context, self.sp - CTX_TEMPFRAME_START_INDEX, self.obj_name(oop));
        }
        self.memory.put_ptr(self.active_context, self.sp, oop);
    }

    pub fn pop(&mut self) -> OOP {
        let top = self.stack_top();
        self.sp -= 1;
        top
    }

    pub fn stack_value(&self, offset: usize) -> OOP {
        let value = self.memory.get_ptr(self.active_context, self.sp - offset);
        if DBG_MEM {
            println!("STCK {:?}_{} -> {}", self.active_context, self.sp as isize - offset as isize - CTX_TEMPFRAME_START_INDEX as isize, self.obj_name(value));
        }
        value
    }

    pub fn stack_top(&self) -> OOP {
        self.stack_value(0)
    }

    pub fn popn(&mut self, count: usize) {
        self.sp -= count;
        if DBG_MEM {
            println!("STCK {:?}_sp <- {}", self.active_context,
                     if self.sp < CTX_TEMPFRAME_START_INDEX {
                         format!("\x1b[1;32m{}\x1b[0m", self.sp as isize - CTX_TEMPFRAME_START_INDEX as isize)
                     } else {
                         format!("{}", self.sp - CTX_TEMPFRAME_START_INDEX)
                     })
        }
    }

    pub fn unpopn(&mut self, count: usize) {
        self.sp += count;
        if DBG_MEM {
            println!("STCK {:?}_sp <- {}", self.active_context, self.sp - CTX_TEMPFRAME_START_INDEX)
        }
    }

    pub fn new_active_context(&mut self, ctx: OOP) {
        self.save_ctx();
        self.memory.dec_ref(self.active_context);
        self.active_context = ctx;
        self.memory.inc_ref(self.active_context);
        self.load_ctx();
    }

    pub fn ctx_sender(&self) -> OOP {
        self.memory.get_ptr(self.home_context, CTX_SENDER_INDEX)
    }

    pub fn ctx_caller(&self) -> OOP {
        self.memory.get_ptr(self.active_context, CTX_SENDER_INDEX)
    }

    pub fn ctx_get_temp(&self, offset: usize) -> OOP {
        let value = self.memory
            .get_ptr(self.home_context, offset + CTX_TEMPFRAME_START_INDEX);
        if DBG_MEM {
            println!("TEMP {:?}_{} -> {}", self.home_context, offset, self.obj_name(value));
        }
        value
    }

    pub fn ctx_put_temp(&mut self, offset: usize, value: OOP) {
        if DBG_MEM {
            println!("TEMP {:?}_{} <- {}", self.home_context, offset, self.obj_name(value));
        }
        self.memory
            .put_ptr(self.home_context, offset + CTX_TEMPFRAME_START_INDEX, value)
    }

    pub fn ctx_literal(&self, offset: usize) -> OOP {
        self.method_literal_of(self.method, offset)
    }
}

// method lookup constants
const SUPERCLASS_INDEX: usize = 0;
const MESSAGE_DICTIONARY_INDEX: usize = 1;
pub const INSTANCE_SPECIFICATION_INDEX: usize = 2;
const METHOD_ARRAY_INDEX: usize = 1;
const SELECTOR_START: usize = 2;

const MESSAGE_SELECTOR_INDEX: usize = 0;
const MESSAGE_ARGUMENTS_INDEX: usize = 1;
const MESSAGE_SIZE: usize = 2;

/// Method lookup
impl Interpreter {
    pub fn oop_hash(&self, oop: OOP) -> usize {
        oop.as_oid() as UWord as usize
    }

    pub fn superclass_of(&self, klass: OOP) -> OOP {
        self.memory.get_ptr(klass, SUPERCLASS_INDEX)
    }

    pub fn lookup_method_in_dict(&mut self, dict: OOP) -> bool {
        let length = self.memory.get_word_length_of(dict);
        let mask = length - SELECTOR_START - 1;
        let index = (mask & self.oop_hash(self.message_selector)) + SELECTOR_START;

        for index in (index..length).into_iter().chain(SELECTOR_START..index) {
            let next_selector = self.memory.get_ptr(dict, index);
            if next_selector == NIL_PTR {
                return false;
            } else if next_selector == self.message_selector {
                let method_array = self.memory.get_ptr(dict, METHOD_ARRAY_INDEX);
                self.new_method = self.memory.get_ptr(method_array, index - SELECTOR_START);
                self.primitive_index = self.primitive_index(self.new_method);
                return true;
            }
        }
        return false;
    }

    pub fn lookup_method_in_class(&mut self, klass: OOP) -> bool {
        let mut current_class = klass;
        while current_class != NIL_PTR {
            if DBG_LOOKUP {
                println!("Looking in {:?}", current_class);
            }
            let dictionary = self.memory.get_ptr(current_class, MESSAGE_DICTIONARY_INDEX);
            if self.lookup_method_in_dict(dictionary) {
                return true;
            }
            current_class = self.superclass_of(current_class);
        }
        if self.message_selector == DOES_NOT_UNDERSTAND_SEL {
            panic!("Recursive not understood error encountered")
        }

        self.create_actual_message();

        println!("[cycle={}] {:depth$} DNU", self.cycle, "", depth = self.call_depth);
        self.message_selector = DOES_NOT_UNDERSTAND_SEL;
        return self.lookup_method_in_class(klass);
    }

    pub fn create_actual_message(&mut self) {
        let protector = self.oop_protector();
        let argument_array = protector.push(self.instantiate_class(
            CLASS_ARRAY_PTR,
            self.argument_count,
            ObjectLayout::Pointer,
        ));

        let message = protector.push(
            self.instantiate_class(CLASS_MESSAGE_PTR, MESSAGE_SIZE, ObjectLayout::Pointer),
        );

        self.memory
            .put_ptr(message, MESSAGE_SELECTOR_INDEX, self.message_selector);
        self.memory
            .put_ptr(message, MESSAGE_ARGUMENTS_INDEX, argument_array);

        self.memory.transfer_fields(
            self.argument_count,
            self.active_context,
            self.sp - self.argument_count + 1,
            argument_array,
            0,
        );
        self.popn(self.argument_count);
        self.push(message);
        self.argument_count = 1;
    }
}

// Instance specification
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
pub(crate) struct InstanceSpecification(usize);

impl InstanceSpecification {
    pub fn is_pointers(self) -> bool {
        self.0 & 0x4000 != 0
    }
    pub fn is_words(self) -> bool {
        self.0 & 0x2000 != 0
    }
    pub fn is_indexable(self) -> bool {
        self.0 & 0x1000 != 0
    }
    pub fn fixed_fields(self) -> usize {
        self.0 & 0x7FF
    }
}

impl From<OOP> for InstanceSpecification {
    fn from(oop: OOP) -> Self {
        InstanceSpecification(oop.as_integer() as UWord as usize)
    }
}

impl Interpreter {
    pub(crate) fn instance_specification(&self, klass: OOP) -> InstanceSpecification {
        self.memory
            .get_ptr(klass, INSTANCE_SPECIFICATION_INDEX)
            .into()
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
pub enum Insn {
    PushReceiverVar(usize),
    PushTemporary(usize),
    PushConst(OOP),
    PushLiteralConst(usize),
    PushLiteralVar(usize),
    PopReceiverVar(usize),
    PopTemporary(usize),
    PopLiteralVar(usize),
    PushReceiver,
    MessageReturnRcvr,
    MessageReturnOOP(OOP),
    MessageReturn,
    BlockReturn,
    StoreReceiverVar(usize),
    StoreTemporary(usize),
    StoreLiteralVar(usize),
    SendLiteral(usize, usize), // selector, arguments
    SendLiteralSuper(usize, usize),
    Pop,
    Dup,
    PushCtx,
    Jump(isize),
    JumpFalse(isize), // pop and jump
    JumpTrue(isize),
    SendSpecial(usize),

    Illegal1([u8; 1]),
    Illegal2([u8; 2]),
    Illegal3([u8; 3]),
}

/// The actual interpreter
impl Interpreter {
    pub fn fetch_insn_byte(&mut self) -> u8 {
        let insn = self.memory.get_byte(self.method, self.ip);
        self.ip += 1;
        insn
    }

    pub fn interpret(&mut self) {
        loop {
            self.cycle();

            let bmark_time = self.startup_time.elapsed().as_secs() / 5;
            if bmark_time != self.bmark_lastprint {
                self.bmark_lastprint = bmark_time;
                let cycles = (self.cycle - self.bmark_cycles)/5;
                self.bmark_cycles = self.cycle;
                println!("BMRK Cycles/s: {}", cycles);

            }
        }
    }

    pub fn cycle(&mut self) {
        self.check_process_switch();
        let (insn, sz) = Self::decode_insn(self.memory.get_bytes(self.method), self.ip);
        self.ip += sz;
        self.cycle += 1;
        if DBG_INSN {
            println!("[cycle={}] Insn: {:?}", self.cycle, insn);
        }
        self.dispatch(insn);
    }

    pub fn decode_insn(bytecode: &[u8], ip: usize) -> (Insn, usize) {
        let mut fetch_ip = ip;
        let mut next_byte = || {
            let byte = bytecode[fetch_ip];
//            println!("Insn byte: {:3} (0x{:x})", byte, byte);
            fetch_ip += 1;
            byte
        };
        let insn = next_byte();
        let decoded = match insn {
            0x00...0x0F => Insn::PushReceiverVar(insn as usize & 0xF),
            0x10...0x1F => Insn::PushTemporary(insn as usize & 0xF),
            0x20...0x3F => Insn::PushLiteralConst(insn as usize & 0x1F),
            0x40...0x5F => Insn::PushLiteralVar(insn as usize & 0x1F),
            0x60...0x67 => Insn::PopReceiverVar(insn as usize & 0x07),
            0x68...0x6F => Insn::PopTemporary(insn as usize & 0x07),
            0x70 => Insn::PushReceiver,
            0x71 => Insn::PushConst(TRUE_PTR),
            0x72 => Insn::PushConst(FALSE_PTR),
            0x73 => Insn::PushConst(NIL_PTR),
            0x74 => Insn::PushConst(OOP::from(-1)),
            0x75 => Insn::PushConst(OOP::from(0)),
            0x76 => Insn::PushConst(OOP::from(1)),
            0x77 => Insn::PushConst(OOP::from(2)),
            0x78 => Insn::MessageReturnRcvr,
            0x79 => Insn::MessageReturnOOP(TRUE_PTR),
            0x7A => Insn::MessageReturnOOP(FALSE_PTR),
            0x7B => Insn::MessageReturnOOP(NIL_PTR),
            0x7C => Insn::MessageReturn,
            0x7D => Insn::BlockReturn,
            0x7E...0x7F => Insn::Illegal1([insn]),
            0x80 => {
                let next = next_byte();
                let sub = next as usize & 0x3F;
                match next & 0xC0 {
                    0x00 => Insn::PushReceiverVar(sub),
                    0x40 => Insn::PushTemporary(sub),
                    0x80 => Insn::PushLiteralConst(sub),
                    0xC0 => Insn::PushLiteralVar(sub),
                    _ => unreachable!(),
                }
            }
            0x81 => {
                let next = next_byte();
                let sub = next as usize & 0x3F;
                match next & 0xC0 {
                    0x00 => Insn::StoreReceiverVar(sub),
                    0x40 => Insn::StoreTemporary(sub),
                    0x80 => Insn::Illegal2([insn, next]),
                    0xC0 => Insn::StoreLiteralVar(sub),
                    _ => unreachable!(),
                }
            }
            0x82 => {
                let next = next_byte();
                let sub = next as usize & 0x3F;
                match next & 0xC0 {
                    0x00 => Insn::PopReceiverVar(sub),
                    0x40 => Insn::PopTemporary(sub),
                    0x80 => Insn::Illegal2([insn, next]),
                    0xC0 => Insn::PopLiteralVar(sub),
                    _ => unreachable!(),
                }
            }
            0x83 => {
                let next = next_byte() as usize;
                Insn::SendLiteral(next & 0x1F, next >> 5)
            }
            0x84 => {
                let args = next_byte() as usize;
                let sel = next_byte() as usize;
                Insn::SendLiteral(sel, args)
            }
            0x85 => {
                let next = next_byte() as usize;
                Insn::SendLiteralSuper(next & 0x1F, next >> 5)
            }
            0x86 => {
                let args = next_byte() as usize;
                let sel = next_byte() as usize;
                Insn::SendLiteralSuper(sel, args)
            }
            0x87 => Insn::Pop,
            0x88 => Insn::Dup,
            0x89 => Insn::PushCtx,
            0x8A...0x8F => Insn::Illegal1([insn]),
            0x90...0x97 => Insn::Jump((insn as isize & 0x7) + 1),
            0x98...0x9F => Insn::JumpFalse((insn as isize & 0x7) + 1),
            0xA0...0xA7 => {
                let next = next_byte() as isize;
                Insn::Jump((((insn as isize & 0x7) - 4) << 8) + next)
            }
            0xA8...0xAB => {
                let next = next_byte() as isize;
                Insn::JumpTrue(((insn as isize & 0x3) << 8) + next)
            }
            0xAC...0xAF => {
                let next = next_byte() as isize;
                Insn::JumpFalse(((insn as isize & 0x3) << 8) + next)
            }
            0xB0...0xCF => Insn::SendSpecial(insn as usize - 0xB0),
            0xD0...0xDF => Insn::SendLiteral(insn as usize & 0xF, 0),
            0xE0...0xEF => Insn::SendLiteral(insn as usize & 0xF, 1),
            0xF0...0xFF => Insn::SendLiteral(insn as usize & 0xF, 2),
        };
        (decoded, fetch_ip - ip)
    }

    pub fn dispatch(&mut self, insn: Insn) {
        match insn {
            Insn::PushReceiverVar(i) => {
                // push receiver variable
                self.push(self.memory.get_ptr(self.receiver, i));
            }
            Insn::PushTemporary(i) => {
                // push temporary
                self.push(self.ctx_get_temp(i));
            }
            Insn::PushConst(oop) => self.push(oop),
            Insn::PushLiteralConst(i) => self.push(self.ctx_literal(i)),
            Insn::PushLiteralVar(i) => {
                self.push(self.memory.get_ptr(self.ctx_literal(i), VALUE_INDEX))
            }
            Insn::PopLiteralVar(i) => {
                let value = self.pop();
                let literal = self.ctx_literal(i);
                self.memory.put_ptr(literal, VALUE_INDEX, value)
            }
            Insn::StoreLiteralVar(i) => {
                self.memory
                    .put_ptr(self.ctx_literal(i), VALUE_INDEX, self.stack_top())
            }
            Insn::PopReceiverVar(i) => {
                let value = self.pop();
                self.memory.put_ptr(self.receiver, i, value)
            }
            Insn::StoreReceiverVar(i) => self.memory.put_ptr(self.receiver, i, self.stack_top()),
            Insn::PopTemporary(i) => {
                let value = self.pop();
                self.ctx_put_temp(i, value)
            }
            Insn::StoreTemporary(i) => {
                self.ctx_put_temp(i, self.stack_top());
            },
            Insn::PushReceiver => self.push(self.receiver),
            Insn::PushCtx => self.push(self.active_context),
            Insn::Dup => self.push(self.stack_top()),
            Insn::Pop => {
                self.pop();
            }
            Insn::Jump(i) => {
                self.ip = (self.ip as isize + i) as usize;
                self.interruption_point();
            }
            Insn::JumpFalse(i) => self.conditional_jump(FALSE_PTR, i),
            Insn::JumpTrue(i) => self.conditional_jump(TRUE_PTR, i),
            Insn::BlockReturn => {
                let value = self.pop();
                self.return_value(self.ctx_caller(), value)
            }
            Insn::MessageReturn => {
                let value = self.pop();
                self.return_value(self.ctx_sender(), value)
            }
            Insn::MessageReturnOOP(oop) => self.return_value(self.ctx_sender(), oop),
            Insn::MessageReturnRcvr => self.return_value(self.ctx_sender(), self.receiver),
            Insn::SendLiteral(sel, args) => self.send_selector(self.ctx_literal(sel), args),
            Insn::SendLiteralSuper(sel, args) => {
                self.argument_count = args;
                self.message_selector = self.ctx_literal(sel);
                self.send_selector_to_class(self.superclass_of(self.method_class(self.method)));
            }
            Insn::SendSpecial(sel) => {
                if self.special_selector_primitive_response(sel).is_none() {
                    let selector = self.memory.get_ptr(SPECIAL_SELECTORS_PTR, sel * 2);
                    let count = self
                        .memory
                        .get_ptr(SPECIAL_SELECTORS_PTR, sel * 2 + 1)
                        .as_integer() as UWord as usize;
                    self.send_selector(selector, count);
                }
            }
            Insn::Illegal1(enc) => panic!("Illegal opcode {:?}", enc),
            Insn::Illegal2(enc) => panic!("Illegal opcode {:?}", enc),
            Insn::Illegal3(enc) => panic!("Illegal opcode {:?}", enc),
        }
    }

    fn return_value(&mut self, ctx: OOP, value: OOP) {
        let protect = self.oop_protector();
        self.interruption_point();
        if ctx == NIL_PTR {
            self.push(self.active_context);
            self.push(value);
            self.send_selector(CANNOT_RETURN_SEL, 1);
            return;
        }

        let sender_ip = self.memory.get_ptr(ctx, CTX_IP_INDEX);
        if sender_ip == NIL_PTR {
            self.push(self.active_context);
            self.push(value);
            self.send_selector(CANNOT_RETURN_SEL, 1);
            return;
        }

        if DBG_CALL {
            println!("[cycle={}] {:depth$} RETN {}", self.cycle, "", self.obj_name(value), depth = self.call_depth);
        }
        self.memory.inc_ref(value);
        protect.push(value);
        self.return_to_active_context(ctx);
        self.push(value);
        self.memory.dec_ref(value);
    }

    fn return_to_active_context(&mut self, ctx: OOP) {
        let protect = self.oop_protector();
        protect.push(ctx);
        self.memory.inc_ref(ctx);
        self.nil_context_fields();
        self.memory.dec_ref(ctx);
        self.active_context = ctx;
        self.load_ctx();
    }

    fn nil_context_fields(&mut self) {
        self.memory
            .put_ptr(self.active_context, CTX_SENDER_INDEX, NIL_PTR);
        self.memory
            .put_ptr(self.active_context, CTX_IP_INDEX, NIL_PTR);
    }

    fn send_selector(&mut self, selector: OOP, arg_count: usize) {
        self.message_selector = selector;
        self.argument_count = arg_count;
        let new_receiver = self.stack_value(arg_count);
        self.send_selector_to_class(self.memory.get_class_of(new_receiver));
    }

    fn send_selector_to_class(&mut self, klass: OOP) {
        if DBG_CALL {

            println!("[cycle={}] {:depth$} SEND {}", self.cycle, "", self.print_methodcall(), depth = self.call_depth);
        }
//        println!("Send selector {:?}", read_st_string(&self.memory, self.message_selector));
        self.find_new_method_in_class(klass);
        self.execute_new_method();
    }

    fn find_new_method_in_class(&mut self, klass: OOP) {
        let hash = (self.message_selector.as_raw() ^ klass.as_raw()) & 0xFF;
        let cached: &mut MethodCacheEntry = &mut self.method_cache[hash as UWord as usize];
        let found = cached.selector == self.message_selector && cached.klass == klass;

        if found {
//            println!("Cached method {:?}", cached.new_method);
            self.new_method = cached.new_method;
            self.primitive_index = cached.primitive_index;
        } else {
            self.lookup_method_in_class(klass);
            let cached: &mut MethodCacheEntry = &mut self.method_cache[hash as UWord as usize];
            cached.selector = self.message_selector;
            cached.klass = klass;
            cached.new_method = self.new_method;
            cached.primitive_index = self.primitive_index;
        }
    }

    fn execute_new_method(&mut self) {
//        println!("Executing method {:?}", self.new_method);
        if self.primitive_response().is_none() {
            self.activate_new_method();
            self.interruption_point();
        } else {
//            println!("Handled primitively");
        }
    }

    fn primitive_response(&mut self) -> Option<()> {
        let prim = self.primitive_index;
        if self.primitive_index == 0 {
            let header = self.method_header_of(self.new_method);
            match header.flag_value() {
                HeaderFlag::PrimSelf => {
                    if DBG_INSN {
                        println!("Prim self");
                    }
                    Some(())
                },
                HeaderFlag::PrimReturn => {
                    let field_idx = header.field_index();
                    if DBG_INSN {
                        println!("Prim return {}", field_idx);
                    }
                    let this_rcvr = self.pop();
                    let value = self.memory.get_ptr(this_rcvr, field_idx);
                    self.push(value);
                    Some(())
                }
                _ => None,
            }
        } else {
            let old_context = self.active_context;
            let res = self.dispatch_prim();
            if DBG_CALL {
                let failflag = if res.is_none() {
                    " FAIL".to_string()
                } else if self.active_context != old_context {
                    " (new context)".to_string()
                } else {
                    format!(" => {}", self.obj_name(self.stack_top()))
                };
                println!("[cycle={}] {:depth$} PRIM {}{}",
                         self.cycle, "", prim, failflag,
                         depth = self.call_depth);
            }
            res
        }
    }

    fn activate_new_method(&mut self) {
        let context_size = if self.method_header_of(self.new_method).large_context_flag() {
            64 + CTX_TEMPFRAME_START_INDEX
        } else {
            36 + CTX_TEMPFRAME_START_INDEX
        };
        let new_ctx = self.instantiate_class(CLASS_METHOD_CONTEXT_PTR, context_size, ObjectLayout::Pointer);
        self.memory.put_ptr(new_ctx, CTX_SENDER_INDEX, self.active_context);
        let new_method_header = self.method_header_of(self.new_method);
        self.context_put_ip(new_ctx, new_method_header.initial_ip() as Word);
        self.context_put_sp(new_ctx, new_method_header.temporary_count() as Word);
        self.memory.put_ptr(new_ctx, CTX_METHOD_INDEX, self.new_method);
        self.memory.transfer_fields(
            self.argument_count + 1,
            self.active_context,
            self.sp - self.argument_count,
            new_ctx,
            CTX_RECEIVER_INDEX,
        );
        self.popn(self.argument_count + 1);
        self.new_active_context(new_ctx)
    }

    fn conditional_jump(&mut self, condition: OOP, offset: isize) {
        self.interruption_point();
        let value = self.pop();
        if value == condition {
            self.ip = (self.ip as isize + offset) as usize;
        } else {
            if (value != TRUE_PTR) && (value != FALSE_PTR) {
                self.unpopn(1);
                self.send_selector(MUST_BE_BOOLEAN_SEL, 0);
            }
        }
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Default)]
struct MethodCacheEntry {
    selector: OOP,
    klass: OOP,
    new_method: OOP,
    primitive_index: usize,
}

// primitives
impl Interpreter {
    fn instantiate_class(&mut self, klass: OOP, size: usize, layout: ObjectLayout) -> OOP {
        if self.dbg_alloc.get() {
            println!("ALOC {} {:?} for {:?}", size, layout, klass);
        }
        let new_val = self.memory.instantiate_class(klass, size, layout)
            .or_else(|| {
                self.gc();
                self.memory.instantiate_class(klass, size, layout)
            });
        if let Some(oop) = new_val {
            return oop;
        } else {
            use crate::objectmemory::ImageFormat;
            self.save_ctx();
            self.backtrace();
            crate::objectmemory::text_format::TextFormat::save("crash.dump", &self.memory);
            panic!("Out of OOPs at context {:?}", self.active_context);
        }
    }

    fn read_method_table(&self) -> Option<HashMap<OOP, String>> {
        let mut method_table: HashMap<OOP, String> = HashMap::new();
        let mt_file = File::open("../original-image/method_table").ok()?;
        use std::io::{BufReader, prelude::*};
        let mut r = BufReader::new(mt_file);
        for line in r.lines() {
            let line = line.ok()?;
            let mut fields = line.split('\t');
            let raw_oop = u16::from_str_radix(fields.next()?, 16).ok()?;
            let name = fields.next()?.trim();
            method_table.insert(OOP(raw_oop as Word), name.to_owned());
        }

        Some(method_table)
    }

    fn backtrace(&self) {
        let method_table = self.read_method_table()
            .unwrap_or_default();

        let mut ctx = self.active_context;

        while ctx != NIL_PTR {
            let (block_str, home) = if self.is_block_ctx(ctx) {
                (" [block]", self.memory.get_ptr(ctx, CTX_HOME_INDEX))
            } else {
                ("", ctx)
            };
            let method = self.memory.get_ptr(home, CTX_METHOD_INDEX);
            let ip = self.memory.get_ptr(ctx, CTX_IP_INDEX);
            println!("\t{} @ {}{}",
                method_table.get(&method).map(|method| method.to_owned())
                    .unwrap_or_else(|| format!("{:?}", method)),
                ip.as_integer(),
                block_str,
            );
            ctx = self.memory.get_ptr(ctx, CTX_SENDER_INDEX);
        }
    }

    fn new_float(&mut self, value: f32) -> OOP {
        let obj = self.instantiate_class(CLASS_FLOAT_PTR, 2, ObjectLayout::Word);
        self.memory.put_float(obj, value);
        obj
    }

    fn oop_protector(&self) -> HeldOops {
        HeldOops::new(&self)
    }

    fn gc(&mut self) {
//        println!("GC!");
        self.memory.clear_marks();
        let mut queue = Vec::new();
        queue.push(self.active_context);
        // global dictionary
        queue.push(OOP::pointer(0x12));
        // protected objects
        for level in self.held_objects.borrow().iter() {
            for obj in level.iter() {
                queue.push(*obj)
            }
        }
//        println!("Protection level: {}", self.held_objects.borrow().len());

        self.memory.trace(queue);
        #[allow(unused)]
        let dropped = self.memory.drop_unmarked();
//        println!("Freed {}", dropped);
        let oops_left = self.memory.oops_left();
//        println!("FREE {}", self.memory.oops_left());
        self.dbg_alloc.set(oops_left < 100);
    }

    fn get_integer(&mut self, oop: OOP, field: usize) -> Option<Word> {
        let iptr = self.memory.get_ptr(oop, field);
        if iptr.is_integer() {
            Some(iptr.as_integer())
        } else {
            None
        }
    }

    fn put_integer(&mut self, oop: OOP, field: usize, value: Word) -> Option<()> {
        self.memory
            .put_ptr(oop, field, OOP::try_from_integer(value)?);
        Some(())
    }

    fn pop_integer(&mut self) -> Option<Word> {
        let stack_top = self.pop();
        if stack_top.is_integer() {
            Some(stack_top.as_integer())
        } else {
            None
        }
    }

    fn push_integer(&mut self, int: Word) -> Option<()> {
        self.push(OOP::try_from_integer(int)?);
        Some(())
    }

    fn long_integer_for(&mut self, int: usize) -> OOP {
        let oop = OOP::from(int as Word);
        if oop.as_integer() as isize as usize == int {
            oop
        } else {
            let mut i = 0;
            let mut itmp = int;
            while itmp != 0 {
                itmp >>= 8;
                i += 1;
            }

            if i < 2 {
                i += 2;
            }
            let obj = self.instantiate_class(
                CLASS_LARGE_POSITIVEINTEGER_PTR,
                i,
                ObjectLayout::Byte,
            );
            itmp = int;
            for j in 0..i {
                self.memory.put_byte(obj, j, (itmp & 0xff) as u8);
                itmp >>= 8;
            }
            obj
        }
    }

    fn long_integer_value_of(&self, oop: OOP) -> Option<usize> {
        if oop.is_integer() {
            Some(oop.as_integer() as usize)
        } else if self.memory.get_class_of(oop) == CLASS_LARGE_POSITIVEINTEGER_PTR {
            let mut result = 0;
            for i in 0..self.memory.get_byte_length_of(oop) {
                result = result + (self.memory.get_byte(oop, i) as usize) << (i * 8);
            }
            Some(result)
        } else {
            None
        }
    }

    // Selectors
    fn special_selector_primitive_response(&mut self, sel: usize) -> Option<()> {
        if sel < 16 {
            self.prim_arith(sel)
        } else {
            self.prim_common(sel)
        }
    }

    fn prim_arith(&mut self, sel: usize) -> Option<()> {
        match sel {
            0 => self.prim_add(),
            1 => self.prim_sub(),
            2 => self.prim_lt(),
            3 => self.prim_gt(),
            4 => self.prim_le(),
            5 => self.prim_ge(),
            6 => self.prim_eq(),
            7 => self.prim_ne(),
            8 => self.prim_mul(),
            9 => self.prim_divide(),
            10 => self.prim_mod(),
            11 => self.prim_mk_point(),
            12 => self.prim_bitshift(),
            13 => self.prim_div(),
            14 => self.prim_bitand(),
            15 => self.prim_bitor(),
            _ => panic!("Unimplemented arith primitive"),
        }
    }

    fn prim_common(&mut self, sel: usize) -> Option<()> {
        let argument_count = self.get_integer(SPECIAL_SELECTORS_PTR, sel * 2 + 1)?;
        let rcvr_klass = self
            .memory
            .get_class_of(self.stack_value(argument_count as usize));
        match self.primitive_index {
            22 => self.prim_equiv(),
            23 => self.prim_class(),
            24 => {
                if rcvr_klass == CLASS_METHOD_CONTEXT_PTR || rcvr_klass == CLASS_BLOCK_CONTEXT_PTR {
                    self.prim_block_copy()
                } else {
                    None
                }
            }
            25 | 26 => {
                if rcvr_klass == CLASS_BLOCK_CONTEXT_PTR {
                    self.prim_value()
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn dispatch_prim(&mut self) -> Option<()> {
        match self.primitive_index {
            0...19 => self.dispatch_prim_arith(),
            40...59 => self.dispatch_prim_float(),
            60...67 => self.dispatch_prim_sub_and_stream(),
            68...79 => self.dispatch_prim_storage(),
            80...89 => self.dispatch_prim_control(),
            90...109 => self.dispatch_prim_io(),
            110...127 => self.dispatch_prim_system(),
            128...255 => self.dispatch_prim_private(),
            _ => None,
        }
    }

    fn dispatch_prim_arith(&mut self) -> Option<()> {
        match self.primitive_index {
            1 => self.prim_add(),
            2 => self.prim_sub(),
            3 => self.prim_lt(),
            4 => self.prim_gt(),
            5 => self.prim_le(),
            6 => self.prim_ge(),
            7 => self.prim_eq(),
            8 => self.prim_ne(),
            9 => self.prim_mul(),
            10 => self.prim_divide(),
            11 => self.prim_mod(),
            12 => self.prim_div(),
            13 => self.prim_quo(),
            14 => self.prim_bitand(),
            15 => self.prim_bitor(),
            16 => self.prim_bitxor(),
            17 => self.prim_bitshift(),
            18 => self.prim_mk_point(),
            _ => None,
        }
    }

    fn prim_add(&mut self) -> Option<()> {
        let arg = self.stack_value(0).try_as_integer()?;
        let rcvr = self.stack_value(1).try_as_integer()?;
        let result = OOP::try_from_integer(rcvr.checked_add(arg)?)?;
        self.popn(2);
        self.push(result);
        Some(())
    }

    fn prim_sub(&mut self) -> Option<()> {
        let arg = self.stack_value(0).try_as_integer()?;
        let rcvr = self.stack_value(1).try_as_integer()?;
        let result = OOP::try_from_integer(rcvr.checked_sub(arg)?)?;
        self.popn(2);
        self.push(result);
        Some(())
    }

    fn prim_mul(&mut self) -> Option<()> {
        let arg = self.stack_value(0).try_as_integer()?;
        let rcvr = self.stack_value(1).try_as_integer()?;
        let result = OOP::try_from_integer(rcvr.checked_mul(arg)?)?;
        self.popn(2);
        self.push(result);
        Some(())
    }

    fn prim_divide(&mut self) -> Option<()> {
        let arg = self.stack_value(0).try_as_integer()?;
        let rcvr = self.stack_value(1).try_as_integer()?;
        if arg == 0 {
            return None;
        } else if rcvr % arg != 0 {
            return None;
        }
        let result = OOP::try_from_integer(rcvr / arg)?;
        self.popn(2);
        self.push(result);
        Some(())
    }

    fn prim_mod(&mut self) -> Option<()> {
        // round towards -inf; 0 <= mod < arg
        let mut arg = self.stack_value(0).try_as_integer()?;
        let mut rcvr = self.stack_value(1).try_as_integer()?;

        if arg == 0 {
            return None
        }
        let (_div, rem) = floor_divmod(rcvr, arg);
        let result = OOP::try_from_integer(rem)?;
        self.popn(2);
        self.push(result);
        Some(())
    }

    fn prim_div(&mut self) -> Option<()> {
        // round towards -inf
        let mut arg = self.stack_value(0).try_as_integer()?;
        let mut rcvr = self.stack_value(1).try_as_integer()?;
        if arg == 0 {
            return None;
        }
        let (div, _rem) = floor_divmod(rcvr, arg);
        let result = OOP::try_from_integer(div)?;
        self.popn(2);
        self.push(result);
        Some(())
    }

    fn prim_quo(&mut self) -> Option<()> {
        // round towards 0
        let arg = self.stack_value(0).try_as_integer()?;
        let rcvr = self.stack_value(1).try_as_integer()?;
        if arg == 0 {
            return None;
        }
        let result = OOP::try_from_integer(rcvr / arg)?;
        self.popn(2);
        self.push(result);
        Some(())
    }
}

macro_rules! defprim_compare {
    ($name:ident, $op:tt) => {
    fn $name(&mut self) -> Option<()> {
        let arg = self.stack_value(0).try_as_integer()?;
        let rcvr = self.stack_value(1).try_as_integer()?;
        if DBG_CALL {
            println!(concat!("SEND {} ", stringify!($op), " {}"), rcvr, arg);
        }
        self.popn(2);
        if rcvr $op arg {
            self.push(TRUE_PTR);
        } else {
            self.push(FALSE_PTR);
        }
        Some(())
    }
    }
}

impl Interpreter {
    defprim_compare!(prim_eq, ==);
    defprim_compare!(prim_ne, !=);
    defprim_compare!(prim_lt, <);
    defprim_compare!(prim_gt, >);
    defprim_compare!(prim_le, <=);
    defprim_compare!(prim_ge, >=);

    fn prim_bitand(&mut self) -> Option<()> {
        let arg = self.stack_value(0).try_as_integer()?;
        let rcvr = self.stack_value(1).try_as_integer()?;
        let result = OOP::try_from_integer(rcvr & arg)?;
        self.popn(2);
        self.push(result);
        Some(())
    }
    fn prim_bitor(&mut self) -> Option<()> {
        let arg = self.stack_value(0).try_as_integer()?;
        let rcvr = self.stack_value(1).try_as_integer()?;
        let result = OOP::try_from_integer(rcvr | arg)?;
        self.popn(2);
        self.push(result);
        Some(())
    }
    fn prim_bitxor(&mut self) -> Option<()> {
        let arg = self.stack_value(0).try_as_integer()?;
        let rcvr = self.stack_value(1).try_as_integer()?;
        let result = OOP::try_from_integer(rcvr ^ arg)?;
        self.popn(2);
        self.push(result);
        Some(())
    }
    fn prim_bitshift(&mut self) -> Option<()> {
        let arg = self.stack_value(0).try_as_integer()?;
        let rcvr = self.stack_value(1).try_as_integer()?;
        let result = if arg < 0 {
            let arg = -arg as usize;
            // TODO: Word size dependent
            if arg > 15 {
                // This will shift in the sign bit
                OOP::try_from_integer(rcvr >> 15)?
            } else {
                OOP::try_from_integer(rcvr >> arg)?
            }
        } else {
            let arg = arg as usize;
            if arg > 15 {
                return None
            }
            let res = OOP::try_from_integer(rcvr << arg)?;
            if res.as_integer() >> arg == rcvr {
                res
            } else {
                return None;
            }
        };
        self.popn(2);
        self.push(result);
        Some(())
    }
}

const CLASS_POINT_SIZE: usize = 2;
const CLASS_POINT_X: usize = 0;
const CLASS_POINT_Y: usize = 1;

impl Interpreter {
    fn prim_mk_point(&mut self) -> Option<()> {
        let arg = self.stack_value(0);
        let rcvr = self.stack_value(1);
        arg.try_as_integer()?;
        rcvr.try_as_integer()?;

        let result =
            self.instantiate_class(CLASS_POINT_PTR, CLASS_POINT_SIZE, ObjectLayout::Pointer);
        self.memory.put_ptr(result, CLASS_POINT_X, rcvr);
        self.memory.put_ptr(result, CLASS_POINT_Y, arg);
        self.popn(2);
        self.push(result);
        Some(())
    }
}

macro_rules! defprim_flt_compare {
    ($name:ident, $op:tt) => {
    fn $name(&mut self) -> Option<()> {
        let arg = self.get_float(self.stack_value(0))?;
        let rcvr = self.get_float(self.stack_value(1))?;
        self.popn(2);
        if rcvr $op arg {
            self.push(TRUE_PTR);
        } else {
            self.push(FALSE_PTR);
        }
        Some(())
    }
    }
}

macro_rules! defprim_flt_arith {
    ($name:ident, $op:tt) => {
    fn $name(&mut self) -> Option<()> {
        let arg = self.get_float(self.stack_value(0))?;
        let rcvr = self.get_float(self.stack_value(1))?;
        let res = self.new_float(rcvr $op arg);
        self.popn(2);
        self.push(res);
        Some(())
    }
    }
}

// Floating point
impl Interpreter {
    fn get_float(&self, oop: OOP) -> Option<f32> {
        if oop.is_integer() {
            return Some(oop.as_integer() as f32);
        }
        return self.memory.get_float(oop);
    }
    fn dispatch_prim_float(&mut self) -> Option<()> {
        match self.primitive_index {
            40 => self.prim_as_float(),
            41 => self.prim_float_add(),
            42 => self.prim_float_sub(),
            43 => self.prim_float_lt(),
            44 => self.prim_float_gt(),
            45 => self.prim_float_le(),
            46 => self.prim_float_ge(),
            47 => self.prim_float_eq(),
            48 => self.prim_float_ne(),
            49 => self.prim_float_mul(),
            50 => self.prim_float_div(),
            51 => self.prim_float_trunc(),
            52 => self.prim_float_frac(),
            53 => self.prim_float_exp(),
            54 => self.prim_float_times_two_power(),
            _ => None,
        }
    }

    fn prim_as_float(&mut self) -> Option<()> {
        let int = self.stack_value(0).try_as_integer()?;
        let float = self.new_float(int as f32);
        self.popn(1);
        self.push(float);
        Some(())
    }

    defprim_flt_arith!(prim_float_add, +);
    defprim_flt_arith!(prim_float_sub, -);
    defprim_flt_arith!(prim_float_mul, *);
    defprim_flt_arith!(prim_float_div, /);
    defprim_flt_compare!(prim_float_lt, <);
    defprim_flt_compare!(prim_float_gt, >);
    defprim_flt_compare!(prim_float_le, <=);
    defprim_flt_compare!(prim_float_ge, >=);
    defprim_flt_compare!(prim_float_eq, ==);
    defprim_flt_compare!(prim_float_ne, !=);

    fn prim_float_trunc(&mut self) -> Option<()> {
        use crate::objectmemory::{SMALLINT_MAX, SMALLINT_MIN};
        let rcvr = self.get_float(self.stack_top())?.trunc();
        if rcvr < SMALLINT_MIN as f32 || rcvr > SMALLINT_MAX as f32 {
            return None;
        }
        let int = OOP::try_from_integer(rcvr as Word)?;
        self.popn(1);
        self.push(int);
        Some(())
    }

    fn prim_float_frac(&mut self) -> Option<()> {
        let rcvr = self.get_float(self.stack_top())?;
        let frac = self.new_float(rcvr.fract());
        self.popn(1);
        self.push(frac);
        Some(())
    }

    fn prim_float_exp(&mut self) -> Option<()> {
        let rcvr = self.get_float(self.stack_top())?;
        let raw_exp = (rcvr.to_bits() >> 23) & 0xFF;
        let unbiased_exp = raw_exp as Word - 127; // safe because 0xFF will always fit into a word
        self.popn(1);
        self.push(OOP::from(unbiased_exp));
        Some(())
    }

    fn prim_float_times_two_power(&mut self) -> Option<()> {
        let rcvr = self.get_float(self.stack_value(1))?;
        let arg = self.stack_value(0);
        let scale = if arg.is_integer() {
            (arg.as_integer() as f32).exp2()
        } else {
            self.get_float(arg)?.exp2()
        };
        let result = self.new_float(scale * rcvr);
        self.popn(2);
        self.push(result);
        Some(())
    }
}

// Array and stream primitives

const STREAM_ARRAY_INDEX: usize = 0;
const STREAM_INDEX_INDEX: usize = 1;
const STREAM_READ_LIMIT_INDEX: usize = 2;
const STREAM_WRITE_LIMIT_INDEX: usize = 3;

impl Interpreter {
    fn dispatch_prim_sub_and_stream(&mut self) -> Option<()> {
        match self.primitive_index {
            60 => self.prim_at(),
            61 => self.prim_atput(),
            62 => self.prim_size(),
            63 => self.prim_string_at(),
            64 => self.prim_string_atput(),
            65 => self.prim_next(),
            66 => self.prim_nextput(),
            67 => self.prim_atend(),
            _ => None,
        }
    }

    fn check_indexable_bounds(&self, index: usize, array: OOP) -> Option<()> {
        let klass = self.memory.get_class_of(array);
        if index < 1 {
            return None;
        }
        // Q: Is this correct? It seems fixed fields might all be pointers (e.g., CompiledMethod)
        // A: CompiledMethod is a bytes object; the other fields have image-level support
        if index + self.instance_specification(klass).fixed_fields() <= self.length_of(klass, array)
        {
            return Some(());
        } else {
            return None;
        }
    }

    fn length_of(&self, klass: OOP, array: OOP) -> usize {
        if self.instance_specification(klass).is_words() {
            self.memory.get_word_length_of(array)
        } else {
            self.memory.get_byte_length_of(array)
        }
    }

    fn vm_at(&mut self, array: OOP, index: usize) -> OOP {
        let klass = self.memory.get_class_of(array);
        let ispec = self.instance_specification(klass);
        if ispec.is_words() {
            if ispec.is_pointers() {
                self.memory.get_ptr(array, index - 1)
            } else {
                self.long_integer_for(self.memory.get_word(array, index - 1) as UWord as usize)
            }
        } else {
            OOP::from(self.memory.get_byte(array, index - 1) as Word)
        }
    }

    fn vm_atput(&mut self, array: OOP, index: usize, value: OOP) -> Option<()> {
        let klass = self.memory.get_class_of(array);
        let ispec = self.instance_specification(klass);
        if ispec.is_words() {
            if ispec.is_pointers() {
                self.memory.put_ptr(array, index - 1, value)
            } else {
                self.memory
                    .put_word(array, index - 1, self.long_integer_value_of(value)? as Word);
            }
        } else {
            self.memory
                .put_byte(array, index - 1, value.try_as_integer()? as u8);
        }
        Some(())
    }

    fn prim_at(&mut self) -> Option<()> {
        let index = self.long_integer_value_of(self.stack_value(0))?;
        let array = self.stack_value(1);
        let klass = self.memory.get_class_of(array);
        self.check_indexable_bounds(index, array)?;
        let index = index + self.instance_specification(klass).fixed_fields();
        let result = self.vm_at(array, index);
        self.popn(2);
        self.push(result);
        Some(())
    }

    fn prim_atput(&mut self) -> Option<()> {
        let value = self.stack_value(0);
        let index = self.long_integer_value_of(self.stack_value(1))?;
        let array = self.stack_value(2);
        let klass = self.memory.get_class_of(array);
        self.check_indexable_bounds(index, array)?;
        let index = index + self.instance_specification(klass).fixed_fields();
        self.vm_atput(array, index, value)?;
        self.popn(3);
        self.push(value);
        Some(())
    }

    fn prim_size(&mut self) -> Option<()> {
        let array = self.stack_top();
        let klass = self.memory.get_class_of(array);
        let length = self.long_integer_for(
            self.length_of(klass, array) - self.instance_specification(klass).fixed_fields(),
        );
        self.popn(1);
        self.push(length);
        Some(())
    }

    fn prim_string_at(&mut self) -> Option<()> {
        let index = self.long_integer_value_of(self.stack_value(0))?;
        let array = self.stack_value(1);
        self.check_indexable_bounds(index, array)?;
        let ascii = self.vm_at(array, index).try_as_integer()?;
        let chr = self.memory.get_ptr(CHARACTER_TABLE_PTR, ascii as usize);
        self.popn(2);
        self.push(chr);
        Some(())
    }

    fn prim_string_atput(&mut self) -> Option<()> {
        let character = self.stack_value(0);
        let index = self.long_integer_value_of(self.stack_value(1))?;
        let array = self.stack_value(2);
        self.check_indexable_bounds(index, array)?;
        if self.memory.get_class_of(character) != CLASS_CHARACTER_PTR {
            return None;
        }
        let ascii = self.memory.get_ptr(character, 0);
        self.vm_atput(array, index, ascii)?;

        self.popn(3);
        self.push(character);
        Some(())
    }

    fn prim_next(&mut self) -> Option<()> {
        let stream = self.stack_top();
        let array = self.memory.get_ptr(stream, STREAM_ARRAY_INDEX);
        let array_klass = self.memory.get_class_of(array);
        let index = self.get_integer(stream, STREAM_INDEX_INDEX)?;
        let limit = self.get_integer(stream, STREAM_READ_LIMIT_INDEX)?;
        if index >= limit {
            return None;
        }
        if array_klass != CLASS_ARRAY_PTR && array_klass != CLASS_STRING_PTR {
            return None;
        }
        let index = index + 1;
        self.check_indexable_bounds(index as usize, array)?;
        let result = self.vm_at(array, index as usize);
        self.put_integer(stream, STREAM_INDEX_INDEX, index)?;
        self.popn(1);
        if array_klass == CLASS_ARRAY_PTR {
            self.push(result);
        } else {
            let char = self
                .memory
                .get_ptr(CHARACTER_TABLE_PTR, result.as_integer() as usize);
            self.push(char);
        }
        Some(())
    }

    fn prim_nextput(&mut self) -> Option<()> {
        let value = self.stack_value(0);
        let stream = self.stack_value(1);
        let array = self.memory.get_ptr(stream, STREAM_ARRAY_INDEX);
        let array_klass = self.memory.get_class_of(array);
        let index = self.get_integer(stream, STREAM_INDEX_INDEX)?;
        let limit = self.get_integer(stream, STREAM_WRITE_LIMIT_INDEX)?;
        if index >= limit {
            return None;
        }
        if array_klass != CLASS_ARRAY_PTR && array_klass != CLASS_STRING_PTR {
            return None;
        }
        let index = index + 1;
        self.check_indexable_bounds(index as usize, array)?;
        if array_klass == CLASS_ARRAY_PTR {
            self.vm_atput(array, index as usize, value);
        } else {
            let ascii = self.memory.get_ptr(value, 0);
            self.vm_atput(array, index as usize, ascii);
        }

        self.put_integer(stream, STREAM_INDEX_INDEX, index)?;
        self.popn(2);
        self.push(value);
        Some(())
    }

    fn prim_atend(&mut self) -> Option<()> {
        let stream = self.stack_top();
        let array = self.memory.get_ptr(stream, STREAM_ARRAY_INDEX);
        let array_klass = self.memory.get_class_of(array);
        let length = self.length_of(array_klass, array);
        let index = self.get_integer(stream, STREAM_INDEX_INDEX)?;
        let limit = self.get_integer(stream, STREAM_READ_LIMIT_INDEX)?;
        if array_klass != CLASS_ARRAY_PTR && array_klass != CLASS_STRING_PTR {
            return None;
        }
        self.popn(1);
        if index >= limit || index as usize >= length {
            self.push(TRUE_PTR);
        } else {
            self.push(FALSE_PTR);
        }

        Some(())
    }
}

// Storage primitives
impl Interpreter {
    fn dispatch_prim_storage(&mut self) -> Option<()> {
        match self.primitive_index {
            68 => self.prim_object_at(),
            69 => self.prim_object_atput(),
            70 => self.prim_new(),
            71 => self.prim_new_with_arg(),
            72 => self.prim_become(),
            73 => self.prim_inst_var_at(),
            74 => self.prim_inst_var_atput(),
            75 => self.prim_as_oop(),
            76 => self.prim_as_object(),
            77 => self.prim_some_instance(),
            78 => self.prim_next_instance(),
            79 => self.prim_new_method(),
            _ => None,
        }
    }

    fn prim_object_at(&mut self) -> Option<()> {
        let index = self.stack_top().try_as_integer()?;
        let receiver = self.stack_value(1);
        if index <= 0 || index as usize > self.method_header_of(receiver).oop_count() {
            return None;
        }
        self.popn(2);
        self.push(self.memory.get_ptr(receiver, index as usize - 1));
        Some(())
    }

    fn prim_object_atput(&mut self) -> Option<()> {
        let value = self.stack_value(0);
        let index = self.stack_value(1).try_as_integer()?;
        let receiver = self.stack_value(2);
        if index <= 0 || index as usize > self.method_header_of(receiver).oop_count() {
            return None;
        }
        self.memory.put_ptr(receiver, index as usize - 1, value);
        self.popn(2);
        self.push(value);
        Some(())
    }

    fn prim_new(&mut self) -> Option<()> {
        let class = self.stack_value(0);
        let ispec = self.instance_specification(class);
        let size = ispec.fixed_fields();
        if ispec.is_indexable() {
            return None;
        }
        let obj = if ispec.is_pointers() {
            self.instantiate_class(class, size, ObjectLayout::Pointer)
        } else if ispec.is_words() {
            self.instantiate_class(class, size, ObjectLayout::Word)
        } else {
            return None;
        };

        self.popn(1);
        self.push(obj);
        Some(())
    }

    fn prim_new_with_arg(&mut self) -> Option<()> {
        let size = self.long_integer_value_of(self.stack_value(0))?;
        let class = self.stack_value(1);

        let ispec = self.instance_specification(class);
        if !ispec.is_indexable() {
            return None;
        }

        let size = size + ispec.fixed_fields();
        let layout = if ispec.is_pointers() {
            ObjectLayout::Pointer
        } else if ispec.is_words() {
            ObjectLayout::Word
        } else {
            ObjectLayout::Byte
        };
        let obj = self.instantiate_class(class, size, layout);

        self.popn(2);
        self.push(obj);
        Some(())
    }

    fn prim_become(&mut self) -> Option<()> {
        let other = self.stack_value(0);
        let this = self.stack_value(1);
        if other.is_integer() || this.is_integer() {
            return None;
        }
        self.memory.swap_pointers(this, other);
        self.popn(1);
        Some(())
    }

    fn check_ivar_bounds_of(&self, object: OOP, index: usize) -> Option<()> {
        if index >= 1 && index <= self.length_of(self.memory.get_class_of(object), object) {
            Some(())
        } else {
            None
        }
    }

    fn prim_inst_var_at(&mut self) -> Option<()> {
        let index = self.stack_value(0).try_as_integer()? as usize;
        let receiver = self.stack_value(1);
        self.check_ivar_bounds_of(receiver, index as usize)?;
        let obj = self.vm_at(receiver, index);
        self.popn(2);
        self.push(obj);
        Some(())
    }

    fn prim_inst_var_atput(&mut self) -> Option<()> {
        let value = self.stack_value(0);
        let index = self.stack_value(1).try_as_integer()? as usize;
        let receiver = self.stack_value(2);
        self.check_ivar_bounds_of(receiver, index as usize)?;
        self.vm_atput(receiver, index, value);
        self.popn(3);
        self.push(value);
        Some(())
    }

    fn prim_as_object(&mut self) -> Option<()> {
        let rcvr = self.stack_value(0);
        if rcvr.is_integer() {
            self.popn(1);
            self.push(rcvr.to_pointer());
            Some(())
        } else {
            None
        }
    }

    fn prim_as_oop(&mut self) -> Option<()> {
        let rcvr = self.stack_top();
        if rcvr.is_object() {
            self.popn(1);
            self.push(rcvr.to_smallint());
            Some(())
        } else {
            None
        }
    }

    fn prim_some_instance(&mut self) -> Option<()> {
        let class = self.stack_top();
        self.memory.initial_instance_of(class).map(|obj| {
            self.pop();
            self.push(obj);
        })
    }

    fn prim_next_instance(&mut self) -> Option<()> {
        let obj = self.stack_top();
        self.memory.next_instance_of(obj).map(|obj| {
            self.pop();
            self.push(obj);
        })
    }

    fn prim_new_method(&mut self) -> Option<()> {
        let header = self.stack_value(0);
        if !header.is_integer() {
            return None;
        }
        let parsed_header = MethodHeader::new(header);
        if parsed_header.flag_value() == HeaderFlag::HeaderExt && parsed_header.literal_count() < 2 {
            return None
        }
        let bytecode_count = self.stack_value(1).try_as_integer()?;
        let class = self.stack_value(2);
        if bytecode_count < 0 {
            return None;
        }

        let size = (parsed_header.literal_count() + 1) * OOP::byte_size()
            + bytecode_count as usize;
        let method = self.instantiate_class(class, size, ObjectLayout::Byte);
        self.memory.put_ptr(method, 0, header);
        if parsed_header.flag_value() == HeaderFlag::HeaderExt {
            // make sure that the header is valid, if existing
            self.memory.put_ptr(method, parsed_header.literal_count() + LITERAL_START - 2, OOP::from(0));
        }
        self.popn(3);
        self.push(method);
        Some(())
    }
}

// Control primitives
impl Interpreter {
    fn dispatch_prim_control(&mut self) -> Option<()> {
        match self.primitive_index {
            80 => self.prim_block_copy(),
            81 => self.prim_value(),
            82 => self.prim_value_with_args(),
            83 => self.prim_perform(),
            84 => self.prim_perform_with_args(),
            85 => self.prim_signal(),
            86 => self.prim_wait(),
            87 => self.prim_resume(),
            88 => self.prim_suspend(),
            89 => self.prim_flush_cache(),
            _ => None,
        }
    }

    fn prim_block_copy(&mut self) -> Option<()> {
        let block_argcount = self.stack_value(0);
        let ctx = self.stack_value(1);
        let method_ctx = if self.is_block_ctx(ctx) {
            self.memory.get_ptr(ctx, CTX_HOME_INDEX)
        } else {
            ctx
        };

        let ctx_size = self.memory.get_word_length_of(method_ctx);
        let new_ctx =
            self.instantiate_class(CLASS_BLOCK_CONTEXT_PTR, ctx_size, ObjectLayout::Pointer);
        let iip = OOP::from(self.ip as i16 + 3);

        self.memory.put_ptr(new_ctx, CTX_INITIAL_IP_INDEX, iip);
        self.memory.put_ptr(new_ctx, CTX_IP_INDEX, iip);
        self.context_put_sp(new_ctx, 0);
        self.memory
            .put_ptr(new_ctx, CTX_BLOCK_ARG_COUNT_INDEX, block_argcount);
        self.memory.put_ptr(new_ctx, CTX_HOME_INDEX, method_ctx);
        self.popn(2);
        self.push(new_ctx);
        Some(())
    }

    fn prim_value(&mut self) -> Option<()> {
        let block_context = self.stack_value(self.argument_count);
        let argcount = self.block_argument_count(block_context);
        if self.argument_count != argcount {
            println!("Wrong argcount (got {}, expected {})", self.argument_count, argcount);
            return None;
        }

        self.memory.transfer_fields(
            argcount,
            self.active_context,
            self.sp + 1 - argcount,
            block_context,
            CTX_TEMPFRAME_START_INDEX,
        );
        self.popn(argcount + 1);
        let iip = self.memory.get_ptr(block_context, CTX_INITIAL_IP_INDEX);
        self.memory.put_ptr(block_context, CTX_IP_INDEX, iip);
        self.context_put_sp(block_context, argcount as Word);
        self.memory
            .put_ptr(block_context, CTX_CALLER_INDEX, self.active_context);
//        self.popn(1);
        self.new_active_context(block_context);
        Some(())
    }

    fn prim_value_with_args(&mut self) -> Option<()> {
        let arg_array = self.stack_value(0);
        let block_ctx = self.stack_value(1);
        let block_argcount = self.block_argument_count(block_ctx);
        let array_class = self.memory.get_class_of(arg_array);
        if array_class != CLASS_ARRAY_PTR {
            return None;
        }
        let array_argcount = self.memory.get_word_length_of(arg_array);
        if array_argcount != block_argcount {
            return None;
        }

        self.memory.transfer_fields(
            array_argcount,
            arg_array,
            0,
            block_ctx,
            CTX_TEMPFRAME_START_INDEX,
        );
        let iip = self.memory.get_ptr(block_ctx, CTX_INITIAL_IP_INDEX);
        self.memory.put_ptr(block_ctx, CTX_IP_INDEX, iip);
        self.context_put_sp(block_ctx, array_argcount as Word);
        self.memory
            .put_ptr(block_ctx, CTX_CALLER_INDEX, self.active_context);
        self.popn(2);
        self.new_active_context(block_ctx);
        Some(())
    }

    fn prim_perform(&mut self) -> Option<()> {
        let perform_selector = self.message_selector;
        self.message_selector = self.stack_value(self.argument_count - 1);
        let new_rcvr = self.stack_value(self.argument_count);

        self.lookup_method_in_class(self.memory.get_class_of(new_rcvr));
        if self.argument_count(self.new_method) != self.argument_count - 1 {
            self.message_selector = perform_selector;
            return None;
        } else {
            let selector_index = self.sp - self.argument_count + 1;
            self.memory.transfer_fields(
                self.argument_count - 1,
                self.active_context,
                selector_index + 1,
                self.active_context,
                selector_index,
            );
            self.popn(1);
            self.argument_count -= 1;
            self.execute_new_method();
            return Some(());
        }
    }

    fn prim_perform_with_args(&mut self) -> Option<()> {
        let argument_array = self.stack_value(0);
        let array_size = self.memory.get_word_length_of(argument_array);
        if self.sp + array_size >= self.memory.get_word_length_of(self.active_context) {
            return None;
        } else if self.memory.get_class_of(argument_array) != CLASS_ARRAY_PTR {
            return None;
        }

        self.popn(1);

        let perform_selector = self.message_selector;
        self.message_selector = self.pop();
        let this_rcvr = self.stack_top();
        self.argument_count = array_size;
        for i in 0..self.argument_count {
            self.push(self.memory.get_ptr(argument_array, i));
        }
        self.lookup_method_in_class(self.memory.get_class_of(this_rcvr));
        if self.argument_count(self.new_method) == self.argument_count {
            self.execute_new_method();
            return Some(());
        } else {
            // BUG: really? I think this should be popn
            self.popn(self.argument_count);
            self.push(self.message_selector);
            self.push(argument_array);
            self.argument_count = 2;
            self.message_selector = perform_selector;
            return None;
        }
    }
}

// class ProcessorScheduler
const PROCESS_LISTS_INDEX: usize = 0;
const ACTIVE_PROCESS_INDEX: usize = 1;
// class LinkedList
const FIRST_LINK_INDEX: usize = 0;
const LAST_LINK_INDEX: usize = 1;
// class Semaphore
const EXCESS_SIGNALS_INDEX: usize = 2;
// class Link
const NEXT_LINK_INDEX: usize = 0;
// class Process
const SUSPENDED_CONTEXT_INDEX: usize = 1;
const PRIOTITY_INDEX: usize = 2;
const MY_LIST_INDEX: usize = 3;

// process scheduling
impl Interpreter {
    fn asynchronous_signal(&mut self, semaphore: OOP) {
        if semaphore != NIL_PTR {
            self.semaphore_list.push(semaphore);
        }
    }

    fn synchronous_signal(&mut self, semaphore: OOP) -> Option<()> {
        if semaphore == NIL_PTR {
            println!("Signalled nil");
            return Some(())
        }
        if self.is_empty_list(semaphore) {
            let excess_signals = self
                .memory
                .get_ptr(semaphore, EXCESS_SIGNALS_INDEX)
                .try_as_integer()?;
            self.memory.put_ptr(
                semaphore,
                EXCESS_SIGNALS_INDEX,
                OOP::try_from_integer(excess_signals + 1)?,
            );
            Some(())
        } else {
            let process = self.remove_first_link_of_list(semaphore);
//            println!("Resumed {:?} from semaphore signal", process);
            self.resume(process)
        }
    }

    fn transfer_to(&mut self, process: OOP) {
        self.new_process = Some(process)
    }

    fn check_process_switch(&mut self) {
        while let Some(semaphore) = self.semaphore_list.pop() {
            self.synchronous_signal(semaphore);
        }

        if let Some(process) = self.new_process.take() {
//            println!("Switched process");
            let active_process = self.active_process();
            self.memory
                .put_ptr(active_process, SUSPENDED_CONTEXT_INDEX, self.active_context);
            self.memory
                .put_ptr(self.scheduler_pointer(), ACTIVE_PROCESS_INDEX, process);
            self.new_active_context(self.memory.get_ptr(process, SUSPENDED_CONTEXT_INDEX));
        }
    }

    fn active_process(&self) -> OOP {
        self.new_process.unwrap_or_else(|| {
            self.memory
                .get_ptr(self.scheduler_pointer(), ACTIVE_PROCESS_INDEX)
        })
    }

    fn scheduler_pointer(&self) -> OOP {
        self.memory.get_ptr(SCHEDULER_ASSOCIATION_PTR, VALUE_INDEX)
    }

    fn first_context(&mut self) -> OOP {
        self.new_process = None;
        self.memory
            .get_ptr(self.active_process(), SUSPENDED_CONTEXT_INDEX)
    }

    fn remove_first_link_of_list(&mut self, linked_list: OOP) -> OOP {
        // TODO: refcount unsafe
        let first_link = self.memory.get_ptr(linked_list, FIRST_LINK_INDEX);
        let last_link = self.memory.get_ptr(linked_list, LAST_LINK_INDEX);
        if last_link == first_link {
            self.memory.put_ptr(linked_list, FIRST_LINK_INDEX, NIL_PTR);
            self.memory.put_ptr(linked_list, LAST_LINK_INDEX, NIL_PTR);
        } else {
            let next_link = self.memory.get_ptr(first_link, NEXT_LINK_INDEX);
            self.memory
                .put_ptr(linked_list, FIRST_LINK_INDEX, next_link);
        }

        self.memory.put_ptr(first_link, NEXT_LINK_INDEX, NIL_PTR);
        return first_link;
    }

    fn add_last_link_to_list(&mut self, linked_list: OOP, link: OOP) {
        // TODO: refcount unsafe
        if self.is_empty_list(linked_list) {
            self.memory.put_ptr(linked_list, FIRST_LINK_INDEX, link);
        } else {
            let last_link = self.memory.get_ptr(linked_list, LAST_LINK_INDEX);
            self.memory.put_ptr(last_link, NEXT_LINK_INDEX, link);
        }
        self.memory.put_ptr(linked_list, LAST_LINK_INDEX, link);
        self.memory.put_ptr(link, MY_LIST_INDEX, linked_list);
    }

    fn is_empty_list(&self, linked_list: OOP) -> bool {
        // TODO: refcount unsafe
        self.memory.get_ptr(linked_list, FIRST_LINK_INDEX) == NIL_PTR
    }

    fn wake_highest_priority(&mut self) -> OOP {
        let process_lists = self
            .memory
            .get_ptr(self.scheduler_pointer(), PROCESS_LISTS_INDEX);
        let mut priority = self.memory.get_word_length_of(process_lists);

        loop {
            let process_list = self.memory.get_ptr(process_lists, priority - 1);
            if !self.is_empty_list(process_list) {
//                println!("Woke process at priority {}", priority);
                return self.remove_first_link_of_list(process_list);
            }
            if priority == 0 {
                panic!("No processes left to run");
            }
            priority -= 1;
        }
    }

    fn sleep(&mut self, process: OOP) -> Option<()> {
        let priority = self.get_integer(process, PRIOTITY_INDEX)?;
        if priority < 1 {
            panic!("Priority in the basement: {}", priority);
        }
        let process_lists = self
            .memory
            .get_ptr(self.scheduler_pointer(), PROCESS_LISTS_INDEX);
        let process_list = self.memory.get_ptr(process_lists, priority as usize - 1);
        self.add_last_link_to_list(process_list, process);
        Some(())
    }

    fn suspend_active(&mut self) {
        let process = self.wake_highest_priority();
        self.transfer_to(process);
    }

    fn resume(&mut self, process: OOP) -> Option<()> {
        let active_process = self.active_process();
        let active_priority = self.get_integer(active_process, PRIOTITY_INDEX)?;
        let new_priority = self.get_integer(process, PRIOTITY_INDEX)?;
        if new_priority > active_priority {
            self.sleep(active_process)?;
            self.transfer_to(process);
            Some(())
        } else {
            self.sleep(process)
        }
    }

    fn prim_signal(&mut self) -> Option<()> {
        self.synchronous_signal(self.stack_top())
    }

    fn prim_wait(&mut self) -> Option<()> {
        let rcvr = self.stack_top();
        let excess_signals = self.get_integer(rcvr, EXCESS_SIGNALS_INDEX)?;
        if excess_signals > 0 {
//            println!("Process {:?} waits on {:?}", self.active_process(), rcvr);
            self.put_integer(rcvr, EXCESS_SIGNALS_INDEX, excess_signals - 1)
        } else {
            self.add_last_link_to_list(rcvr, self.active_process());
            self.suspend_active();
            Some(())
        }
    }

    fn prim_resume(&mut self) -> Option<()> {
        self.resume(self.stack_top())
    }

    fn prim_suspend(&mut self) -> Option<()> {
        if self.stack_top() != self.active_process() {
            None
        } else {
            self.pop();
            self.push(NIL_PTR);
            self.suspend_active();
            Some(())
        }
    }

    fn prim_flush_cache(&mut self) -> Option<()> {
        self.method_cache = [MethodCacheEntry::default(); 256];
        Some(())
    }
}

struct DisplayState {
    display: OOP,
    cursor: OOP,
    // if linked, cursor_location is None
    last_event: u128,

    cursor_location: Option<(isize, isize)>,
    mouse_location: (isize, isize),

    mouse_delay_start: Option<Instant>,
    mouse_queued: bool,

    input_semaphore: OOP,
    input_queue: VecDeque<UWord>,

    sample_interval_ms: usize,
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
enum StEvent {
    PointerPos(UWord, UWord),
    /// Device, down
    Bistate(UWord, bool),
}

// IO primitives
impl Interpreter {

    fn push_event_time(&mut self) {
        // push time code
        let elapsed = self.startup_time.elapsed().as_millis();
        let dt = (elapsed - self.display.last_event) as u32;
        self.display.last_event = elapsed;
        if dt == 0 {
            // do nothing
        } else if dt < 0x1000 {
            self.push_event_word(dt as UWord);
        } else {
            let abstime = elapsed as u32;
            self.push_event_word(0x5000);
            self.push_event_word((abstime >> 16) as UWord);
            self.push_event_word(abstime as UWord);
        }
    }

    fn send_mouse_update(&mut self, new_event: bool) {
        self.display.mouse_queued |= new_event;
        if let Some(elapsed) = self.display.mouse_delay_start.as_ref().map(Instant::elapsed) {
            if elapsed.as_millis() as usize > self.display.sample_interval_ms {
                self.display.mouse_delay_start = None;
            }
        }

        if self.display.mouse_delay_start.is_none() && self.display.mouse_queued {
            let (x,y) = self.display.mouse_location;
            self.push_event_time();
            self.push_event_word((x as UWord & 0xFFF) | 0x1000);
            self.push_event_word((y as UWord & 0xFFF) | 0x2000);
            self.display.mouse_queued = false;
            self.display.mouse_delay_start = Some(Instant::now());
        }
    }

    fn push_event(&mut self, event: StEvent) {

        match event {
            StEvent::PointerPos(x, y) => {
                self.display.mouse_location = (x as isize, y as isize);
                self.send_mouse_update(false);
            }
            StEvent::Bistate(dev, down) => {
                self.push_event_time();
                let tag = if down { 0x3000 } else { 0x4000 };
                self.push_event_word(dev | tag);
            }
        }
    }

    fn push_event_word(&mut self, word: UWord) {
//        println!("Sent word {:04x}", word);
        self.display.input_queue.push_back(word);
        self.synchronous_signal(self.display.input_semaphore);
    }

    fn dispatch_prim_io(&mut self) -> Option<()> {
//        println!("Dispatch {}", self.primitive_index);
        match self.primitive_index {
            90 => self.prim_mouse_point(),
            91 => self.prim_cursor_loc_put(),
            92 => self.prim_cursor_link(),
            93 => self.prim_input_semaphore(),
            94 => self.prim_sample_interval(),
            95 => self.prim_input_word(),
            96 => self.prim_copy_bits(), // in bitblt
            97 => self.prim_snapshot(),
            98 => self.prim_time_words_into(),
            99 => self.prim_tick_words_into(),
            100 => self.prim_signal_at_tick(),
            101 => self.prim_be_cursor(),
            102 => self.prim_be_display(),
            103 => self.prim_scan_characters(),
            104 => self.prim_draw_loop(),
            105 => self.prim_string_replace(),
            _ => None,
        }
    }

    fn prim_mouse_point(&mut self) -> Option<()> {
        let pt =
            self.instantiate_class(CLASS_POINT_PTR, CLASS_POINT_SIZE, ObjectLayout::Pointer);
        self.memory.put_ptr(
            pt,
            CLASS_POINT_X,
            OOP::from(self.display.mouse_location.0 as Word),
        );
        self.memory.put_ptr(
            pt,
            CLASS_POINT_Y,
            OOP::from(self.display.mouse_location.1 as Word),
        );
        self.pop(); // pop receiver
        self.push(pt);
        Some(())
    }

    fn prim_cursor_loc_put(&mut self) -> Option<()> {
        let pt = self.stack_top();
        let pt_x = self.get_integer(pt, CLASS_POINT_X)?;
        let pt_y = self.get_integer(pt, CLASS_POINT_Y)?;
        self.pop();

        {
            let target = self
                .display
                .cursor_location
                .as_mut()
                .unwrap_or(&mut self.display.mouse_location);
            target.0 = pt_x as isize;
            target.1 = pt_y as isize;
        }
        Some(())
    }

    fn prim_cursor_link(&mut self) -> Option<()> {
        let flag = self.pop();
        // TODO: Handle non-boolean
        if flag == TRUE_PTR {
            self.display.cursor_location = None;
        } else {
            self.display.cursor_location = Some(self.display.mouse_location);
        }
        Some(())
    }

    fn prim_input_semaphore(&mut self) -> Option<()> {
        // TODO: error handling
        println!("Set input semaphore to {:?}", self.stack_top());
        self.display.input_semaphore = self.pop();
        Some(())
    }

    fn prim_sample_interval(&mut self) -> Option<()> {
        let value = self.stack_top().try_as_integer()?;
        self.pop();
        self.display.sample_interval_ms = value as UWord as usize;
        Some(())
    }

    fn prim_input_word(&mut self) -> Option<()> {
        let word = self.display.input_queue.pop_front()?;
//        println!("Input word {:04x}", word);
        let item = if word >= 0x4000 {
//            println!("Unexpectedly long word");
            self.long_integer_for(word as usize)
        } else {
            OOP::try_from_integer(word as Word)?
        };
        self.pop(); // pop receiver
        self.push(item);
        Some(())
    }

    fn prim_snapshot(&mut self) -> Option<()> {
        println!("Snapshot!");
        // save the active context
        self.save_ctx();
        let proc = self.active_process();
        self.memory.put_ptr(proc, SUSPENDED_CONTEXT_INDEX, self.active_context);
        self.gc();

        use crate::objectmemory::{ImageFormat, text_format::TextFormat};
        if let Err(err) = TextFormat::save("snapshot.dump", &self.memory) {
            eprintln!("Encountered error {}", err);
            None
        } else {
            self.memory.put_ptr(proc, SUSPENDED_CONTEXT_INDEX, NIL_PTR);
            self.pop();
            self.push(NIL_PTR);
            Some(())
            // unset suspended context
        }
    }

    fn prim_time_words_into(&mut self) -> Option<()> {
        let unix_time = ::std::time::SystemTime::now()
            .duration_since(::std::time::UNIX_EPOCH)
            .ok()?
            .as_secs();
        let st_time = unix_time + 2177452800;

        let result_array = self.stack_value(0);
        for i in 0..4 {
            self.vm_atput(
                result_array,
                i+1,
                OOP::try_from_integer(((st_time >> (8 * i)) & 0xFF) as Word)?,
            );
        }
        self.pop();
        // TODO: return result array or self? Right now, returns self
        Some(())
    }

    fn prim_tick_words_into(&mut self) -> Option<()> {
        let unix_time = self.time_millis();

        let result_array = self.stack_value(0);
        for i in 0..4 {
            self.vm_atput(
                result_array,
                i+1,
                OOP::try_from_integer(((unix_time >> (8 * i)) & 0xFF) as Word)?,
            );
        }
        print!("Fetched time: ");
        for i in 0..4 {
            print!("{:02x}", self.memory.get_byte(result_array, i))
        }
        println!();
        self.pop();
        // TODO: return result array or self? Right now, returns self
        Some(())
    }

    fn prim_signal_at_tick(&mut self) -> Option<()> {
        let when_array = self.stack_value(0);
        let semaphore = self.stack_value(1);

        let mut when = 0;
        println!("Scheduling timer... at {}", self.obj_name(when_array));
        let bytelen = self.memory.get_byte_length_of(when_array);
        print!("Raw time: ");
        for i in 0..bytelen {
            let byte = self.memory.get_byte(when_array, i) as u32;
            print!("{:2x}", byte);
            when |= byte << (i * 8);
//            when = (when << 8) | byte;
        }
        println!();
        println!("Scheduled timer for {:8x}", when);
        println!("Now is              {:8x}", self.time_millis());
        self.timer_when = when;
        self.timer_sem = Some(semaphore);
        self.popn(1);
        Some(())
    }

    fn prim_be_cursor(&mut self) -> Option<()> {
        self.display.cursor = self.stack_top();
        Some(())
    }

    fn prim_be_display(&mut self) -> Option<()> {
        self.display.display = self.stack_top();
        self::display::notice_new_display(self);
        Some(())
    }

    fn prim_scan_characters(&mut self) -> Option<()> {
        // TODO: Implement me
        None
    }

    fn prim_draw_loop(&mut self) -> Option<()> {
        // TODO: implement me
        None
    }

    fn prim_string_replace(&mut self) -> Option<()> {
        // TODO: implement me
        None
    }
}

// System primitives
impl Interpreter {
    fn dispatch_prim_system(&mut self) -> Option<()> {
        match self.primitive_index {
            110 => self.prim_equiv(),
            111 => self.prim_class(),
            112 => self.prim_core_left(),
            113 => self.prim_quit(),
            114 => self.prim_debug(),
            115 => self.prim_oops_left(),
            116 => self.prim_signal_at_oops_left_words_left(),
            _ => None,
        }
    }

    fn prim_equiv(&mut self) -> Option<()> {
        let other = self.pop();
        let this = self.pop();
        if this == other {
            self.push(TRUE_PTR);
        } else {
            self.push(FALSE_PTR);
        }
        Some(())
    }

    fn prim_class(&mut self) -> Option<()> {
        let instance = self.pop();
        self.push(self.memory.get_class_of(instance));
        Some(())
    }

    fn prim_core_left(&mut self) -> Option<()> {
        self.pop();
        // more than I can say, for sure...
        let result = self.long_integer_for(0xFFFFFFFF);
        self.push(result);
        Some(())
    }

    fn prim_quit(&mut self) -> Option<()> {
        println!("Exit requested");
        ::std::process::exit(0);
    }

    fn prim_debug(&mut self) -> Option<()> {
        println!("Drop into debugger");
        Some(())
    }

    fn prim_oops_left(&mut self) -> Option<()> {
        self.pop();
        self.push(OOP::from(self.memory.oops_left() as Word));
        Some(())
    }

    fn prim_signal_at_oops_left_words_left(&mut self) -> Option<()> {
        println!("signal:atOopsLeft:wordsLeft:");
        self.popn(3);
        Some(())
    }
}

// Private primitives
impl Interpreter {
    fn dispatch_prim_private(&mut self) -> Option<()> {
        match self.primitive_index {
            // start at 128
            _ => None,
        }
    }
    /*
    fn prim_(&mut self) -> Option<()> {
        Some(())
    }

    fn prim_(&mut self) -> Option<()> {
        Some(())
    }
    */
}

// Regular processing...
impl Interpreter {
    fn time_millis(&self) -> u32 {
//        SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis() as u32
        self.startup_time.elapsed().as_millis() as u32
    }

    fn interruption_point(&mut self) {
        // Any queued semaphores?
        if self.timer_sem.is_some()
            && u32::wrapping_sub(self.time_millis(), self.timer_when) < 0x7FFF_FFFF
        {
            println!("Timer semaphore triggered");
            let sem = self.timer_sem.take().unwrap();
            self.synchronous_signal(sem);
        }

        // Any display processing?
        self::display::poll_display(self);
    }
}

pub fn read_st_string(memory: &ObjectMemory, oop: OOP) -> Cow<str> {
    String::from_utf8_lossy(memory.get_bytes(oop))
}

impl Interpreter {
    pub fn class_name(&self, klass: OOP) -> Cow<str> {


        let name = self.memory.get_ptr(klass, 6); // Class::name
        if self.memory.get_class_of(name) != OOP::pointer(0x1C) {
            return Cow::Borrowed("Wut?");
        } else {
            read_st_string(&self.memory, name)
        }
    }

    pub fn obj_name(&self, obj: OOP) -> String {
        if obj.is_integer() {
            return obj.as_integer().to_string()
        } else if obj == NIL_PTR {
            return "nil".to_string()
        }
        let name = match self.memory.get_class_of(obj) {
            OOP(0x38) => format!("#{}", read_st_string(&self.memory, obj)),
            CLASS_STRING_PTR => format!("{:?}", read_st_string(&self.memory, obj)),
            CLASS_LARGE_POSITIVEINTEGER_PTR => {
                let digits = self.memory.get_bytes(obj);
                let mut result = format!("LargePositiveInteger({}, ", digits.len());
                for digit in digits.iter().rev() {
                    result += &format!("{:02x}", digit);
                }
                result.push(')');
                result
            },
            CLASS_POINT_PTR => {
                let x = self.memory.get_ptr(obj, 0);
                let y = self.memory.get_ptr(obj, 1);
                format!("({} @ {})", self.obj_name(x), self.obj_name(y))
            },
            CLASS_CHARACTER_PTR => {
                let chr = self.memory.get_ptr(obj, 0);
                if chr.is_integer() && chr.as_integer() >= 0 && chr.as_integer() <= 255 {
                    format!("{:?}", chr.as_integer() as u8 as char)
                } else {
                    format!("aCharacter({:?})", chr)
                }
            }
            klass => {
                if self.memory.get_class_of(klass) == OOP::pointer(0x1E) {
                    format!("({} class)", self.class_name(obj))
                } else {
                    format!("a{}", self.class_name(klass))
                }
            }
        };

        format!("{}({:?})", name, obj)
    }

    pub fn print_methodcall(&self) -> String{
        let rcvr = self.stack_value(self.argument_count);
        let mut result =
            format!("{} {}",
                    self.obj_name(rcvr),
                    read_st_string(&self.memory, self.message_selector));
        for i in 0..self.argument_count {
            let arg = self.stack_value(self.argument_count - i - 1);
            result += &format!(" {}", self.obj_name(arg));
        }
        result
    }
}