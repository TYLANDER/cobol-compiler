/// Initialize a sort operation, returning a sort handle.
///
/// # Safety
///
/// - `key_offsets` must point to at least `num_keys` readable `u32` values, or be null.
/// - `key_lengths` must point to at least `num_keys` readable `u32` values, or be null.
/// - `key_ascending` must point to at least `num_keys` readable `bool` values, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_sort_init(
    record_size: u32,
    num_keys: u32,
    key_offsets: *const u32,
    key_lengths: *const u32,
    key_ascending: *const bool,
) -> *mut std::ffi::c_void {
    let _ = (
        record_size,
        num_keys,
        key_offsets,
        key_lengths,
        key_ascending,
    );
    // TODO: implement sort initialization
    std::ptr::null_mut()
}

/// Release (add) a record into the sort.
///
/// # Safety
///
/// - `sort_handle` must be a valid sort handle returned by `cobolrt_sort_init`, or be null.
/// - `record` must point to at least `record_len` readable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_sort_release(
    sort_handle: *mut std::ffi::c_void,
    record: *const u8,
    record_len: u32,
) -> i32 {
    let _ = (sort_handle, record, record_len);
    0
}

/// Return (retrieve) the next sorted record.
///
/// Returns 0 on success, 10 when all records have been returned.
///
/// # Safety
///
/// - `sort_handle` must be a valid sort handle returned by `cobolrt_sort_init`, or be null.
/// - `record` must point to at least `record_len` writable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_sort_return(
    sort_handle: *mut std::ffi::c_void,
    record: *mut u8,
    record_len: u32,
) -> i32 {
    // Returns 0=success, 10=end
    let _ = (sort_handle, record, record_len);
    10
}

/// Finish and clean up a sort operation.
///
/// # Safety
///
/// - `sort_handle` must be a valid sort handle returned by `cobolrt_sort_init`, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_sort_finish(sort_handle: *mut std::ffi::c_void) {
    let _ = sort_handle;
}
