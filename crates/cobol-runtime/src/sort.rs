use std::cmp::Ordering;

/// A single sort key descriptor.
struct SortKeyDesc {
    offset: u32,
    length: u32,
    ascending: bool,
}

/// Internal sort state held behind a `*mut c_void` handle.
struct SortState {
    #[allow(dead_code)]
    record_size: u32,
    keys: Vec<SortKeyDesc>,
    records: Vec<Vec<u8>>,
    /// Index for the next record to return (after sorting).
    return_index: usize,
    /// Whether the records have been sorted yet.
    sorted: bool,
}

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
    let mut keys = Vec::new();
    if !key_offsets.is_null() && !key_lengths.is_null() && !key_ascending.is_null() {
        for i in 0..num_keys as usize {
            keys.push(SortKeyDesc {
                offset: *key_offsets.add(i),
                length: *key_lengths.add(i),
                ascending: *key_ascending.add(i),
            });
        }
    }
    // If no keys provided, sort by entire record ascending
    if keys.is_empty() {
        keys.push(SortKeyDesc {
            offset: 0,
            length: record_size,
            ascending: true,
        });
    }

    let state = Box::new(SortState {
        record_size,
        keys,
        records: Vec::new(),
        return_index: 0,
        sorted: false,
    });
    Box::into_raw(state) as *mut std::ffi::c_void
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
    if sort_handle.is_null() || record.is_null() {
        return -1;
    }
    let state = &mut *(sort_handle as *mut SortState);
    let slice = std::slice::from_raw_parts(record, record_len as usize);
    state.records.push(slice.to_vec());
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
    if sort_handle.is_null() || record.is_null() {
        return 10;
    }
    let state = &mut *(sort_handle as *mut SortState);

    // Sort on first retrieval
    if !state.sorted {
        let keys = &state.keys;
        state.records.sort_by(|a, b| {
            for key in keys {
                let off = key.offset as usize;
                let len = key.length as usize;
                let end_a = std::cmp::min(off + len, a.len());
                let end_b = std::cmp::min(off + len, b.len());
                let slice_a = if off < a.len() { &a[off..end_a] } else { &[] };
                let slice_b = if off < b.len() { &b[off..end_b] } else { &[] };
                let cmp = slice_a.cmp(slice_b);
                if cmp != Ordering::Equal {
                    return if key.ascending { cmp } else { cmp.reverse() };
                }
            }
            Ordering::Equal
        });
        state.sorted = true;
    }

    if state.return_index >= state.records.len() {
        return 10; // end of records
    }

    let rec = &state.records[state.return_index];
    state.return_index += 1;

    let copy_len = std::cmp::min(record_len as usize, rec.len());
    std::ptr::copy_nonoverlapping(rec.as_ptr(), record, copy_len);
    // Pad remainder with spaces
    if (record_len as usize) > copy_len {
        std::ptr::write_bytes(record.add(copy_len), b' ', record_len as usize - copy_len);
    }

    0
}

/// Finish and clean up a sort operation.
///
/// # Safety
///
/// - `sort_handle` must be a valid sort handle returned by `cobolrt_sort_init`, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_sort_finish(sort_handle: *mut std::ffi::c_void) {
    if !sort_handle.is_null() {
        let _ = Box::from_raw(sort_handle as *mut SortState);
    }
}
