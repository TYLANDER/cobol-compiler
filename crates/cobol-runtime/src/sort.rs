/// Sort a file
#[no_mangle]
pub extern "C" fn cobolrt_sort_init(
    record_size: u32,
    num_keys: u32,
    key_offsets: *const u32,
    key_lengths: *const u32,
    key_ascending: *const bool,
) -> *mut std::ffi::c_void {
    let _ = (record_size, num_keys, key_offsets, key_lengths, key_ascending);
    // TODO: implement sort initialization
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn cobolrt_sort_release(
    sort_handle: *mut std::ffi::c_void,
    record: *const u8,
    record_len: u32,
) -> i32 {
    let _ = (sort_handle, record, record_len);
    0
}

#[no_mangle]
pub extern "C" fn cobolrt_sort_return(
    sort_handle: *mut std::ffi::c_void,
    record: *mut u8,
    record_len: u32,
) -> i32 {
    // Returns 0=success, 10=end
    let _ = (sort_handle, record, record_len);
    10
}

#[no_mangle]
pub extern "C" fn cobolrt_sort_finish(sort_handle: *mut std::ffi::c_void) {
    let _ = sort_handle;
}
