use std::cell::RefCell;

thread_local! {
    static PERFORM_STACK: RefCell<Vec<u64>> = RefCell::new(Vec::with_capacity(64));
}

/// Push a return address onto the PERFORM stack
#[no_mangle]
pub extern "C" fn cobolrt_perform_push(return_addr: u64) {
    PERFORM_STACK.with(|stack| {
        stack.borrow_mut().push(return_addr);
    });
}

/// Pop and return the top of the PERFORM stack
/// Returns 0 if stack is empty (should not happen in correct programs)
#[no_mangle]
pub extern "C" fn cobolrt_perform_pop() -> u64 {
    PERFORM_STACK.with(|stack| stack.borrow_mut().pop().unwrap_or(0))
}

/// Get current PERFORM stack depth (for debugging)
#[no_mangle]
pub extern "C" fn cobolrt_perform_depth() -> u32 {
    PERFORM_STACK.with(|stack| stack.borrow().len() as u32)
}
