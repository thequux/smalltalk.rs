use crate::interpreter::{DisplayState, Interpreter, MethodCacheEntry, SUSPENDED_CONTEXT_INDEX};
use crate::objectmemory::{ObjectMemory, NIL_PTR};
use std::collections::{VecDeque, HashSet};
use std::time::Instant;
use std::cell::{RefCell, Cell};
use std::rc::Rc;

impl super::Interpreter {
    pub fn boot(memory: ObjectMemory) -> Self {
        let display = super::display::StDisplay::new();
        let mut interp = Interpreter {
            memory,
            active_context: NIL_PTR,
            home_context: NIL_PTR,
            method: NIL_PTR,
            receiver: NIL_PTR,
            cycle: 0,
            call_depth: 0,
            bmark_cycles: 0,
            bmark_lastprint: 0,
            ip: 0,
            sp: 0,
            message_selector: NIL_PTR,
            argument_count: 0,
            new_method: NIL_PTR,
            primitive_index: 0,
            method_cache: [MethodCacheEntry::default(); 256],
            new_process: None,
            semaphore_list: vec![],
            display: DisplayState {
                display: NIL_PTR,
                cursor: NIL_PTR,
                last_event: 0,
                cursor_location: None,
                mouse_location: (0, 0),
                input_semaphore: NIL_PTR,
                input_queue: VecDeque::new(),
                sample_interval_ms: 0,
                mouse_delay_start: None,
                mouse_queued: false,
            },
            display_impl: display,
            startup_time: Instant::now(),
            timer_sem: None,
            timer_when: 0,
            held_objects: Rc::new(RefCell::new(Vec::new())),
            dbg_alloc: Cell::new(false),
        };

        interp.memory.pad_table();

        // load the context
        interp.active_context = interp
            .memory
            .get_ptr(interp.active_process(), SUSPENDED_CONTEXT_INDEX);
        interp.memory.inc_ref(interp.active_context);
        interp.load_ctx();

        interp.gc();
        interp
    }
}
