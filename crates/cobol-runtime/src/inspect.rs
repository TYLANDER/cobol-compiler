//! Runtime support for COBOL INSPECT and UNSTRING verbs.

/// INSPECT ... TALLYING tally_var FOR ALL|LEADING|CHARACTERS literal
///   [BEFORE|AFTER INITIAL boundary]
///
/// # Safety
///
/// - `data` must point to at least `data_len` readable bytes, or be null.
/// - `tally` must point to at least `tally_len` writable bytes, or be null.
/// - `search` must point to at least `search_len` readable bytes, or be null.
/// - `before_initial` must point to at least `before_initial_len` readable bytes, or be null.
/// - `after_initial` must point to at least `after_initial_len` readable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_inspect_tallying(
    data: *const u8,
    data_len: u32,
    tally: *mut u8,
    tally_len: u32,
    mode: u32,
    search: *const u8,
    search_len: u32,
    before_initial: *const u8,
    before_initial_len: u32,
    after_initial: *const u8,
    after_initial_len: u32,
) {
    if data.is_null() || tally.is_null() {
        return;
    }

    // SAFETY: Caller guarantees `data` points to `data_len` readable bytes.
    let data_slice = std::slice::from_raw_parts(data, data_len as usize);
    // SAFETY: Caller guarantees `tally` points to `tally_len` writable bytes.
    let tally_slice = std::slice::from_raw_parts_mut(tally, tally_len as usize);

    // Read the current tally value (display-format numeric: ASCII digits)
    let mut tally_val: i64 = 0;
    for &b in tally_slice.iter() {
        if b.is_ascii_digit() {
            tally_val = tally_val * 10 + (b - b'0') as i64;
        }
    }

    // Determine the effective range based on BEFORE/AFTER INITIAL
    let start_pos = if !after_initial.is_null() && after_initial_len > 0 {
        // SAFETY: Caller guarantees `after_initial` points to `after_initial_len` readable bytes.
        let boundary = std::slice::from_raw_parts(after_initial, after_initial_len as usize);
        find_substring(data_slice, boundary)
            .map(|pos| pos + boundary.len())
            .unwrap_or(data_slice.len()) // if AFTER boundary not found, no data to inspect
    } else {
        0
    };

    let end_pos = if !before_initial.is_null() && before_initial_len > 0 {
        // SAFETY: Caller guarantees `before_initial` points to `before_initial_len` readable bytes.
        let boundary = std::slice::from_raw_parts(before_initial, before_initial_len as usize);
        find_substring(data_slice, boundary).unwrap_or(data_slice.len())
    } else {
        data_slice.len()
    };

    if start_pos >= end_pos {
        return;
    }

    let effective = &data_slice[start_pos..end_pos];

    // Count based on mode
    let count = match mode {
        0 => {
            // CHARACTERS: count all characters in the effective range
            effective.len() as i64
        }
        1 => {
            // ALL: count all occurrences of the search string
            if search.is_null() || search_len == 0 {
                0
            } else {
                // SAFETY: Caller guarantees `search` points to `search_len` readable bytes.
                let search_slice = std::slice::from_raw_parts(search, search_len as usize);
                count_all_occurrences(effective, search_slice)
            }
        }
        2 => {
            // LEADING: count leading occurrences of the search string
            if search.is_null() || search_len == 0 {
                0
            } else {
                // SAFETY: Caller guarantees `search` points to `search_len` readable bytes.
                let search_slice = std::slice::from_raw_parts(search, search_len as usize);
                count_leading_occurrences(effective, search_slice)
            }
        }
        _ => 0,
    };

    tally_val += count;

    // Write the tally value back (display-format, right-justified, zero-padded)
    write_display_numeric(tally_slice, tally_val);
}

/// INSPECT ... REPLACING ALL|LEADING|FIRST|CHARACTERS search BY replacement
///   [BEFORE|AFTER INITIAL boundary]
///
/// # Safety
///
/// - `data` must point to at least `data_len` writable bytes, or be null.
/// - `search` must point to at least `search_len` readable bytes, or be null.
/// - `replacement` must point to at least `replacement_len` readable bytes, or be null.
/// - `before_initial` must point to at least `before_initial_len` readable bytes, or be null.
/// - `after_initial` must point to at least `after_initial_len` readable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_inspect_replacing(
    data: *mut u8,
    data_len: u32,
    mode: u32,
    search: *const u8,
    search_len: u32,
    replacement: *const u8,
    replacement_len: u32,
    before_initial: *const u8,
    before_initial_len: u32,
    after_initial: *const u8,
    after_initial_len: u32,
) {
    if data.is_null() || replacement.is_null() {
        return;
    }

    // SAFETY: Caller guarantees `data` points to `data_len` writable bytes.
    let data_slice = std::slice::from_raw_parts_mut(data, data_len as usize);
    // SAFETY: Caller guarantees `replacement` points to `replacement_len` readable bytes.
    let repl_slice = std::slice::from_raw_parts(replacement, replacement_len as usize);

    // Determine effective range
    let start_pos = if !after_initial.is_null() && after_initial_len > 0 {
        // SAFETY: Caller guarantees `after_initial` points to `after_initial_len` readable bytes.
        let boundary = std::slice::from_raw_parts(after_initial, after_initial_len as usize);
        find_substring(data_slice, boundary)
            .map(|pos| pos + boundary.len())
            .unwrap_or(data_slice.len())
    } else {
        0
    };

    let end_pos = if !before_initial.is_null() && before_initial_len > 0 {
        // SAFETY: Caller guarantees `before_initial` points to `before_initial_len` readable bytes.
        let boundary = std::slice::from_raw_parts(before_initial, before_initial_len as usize);
        find_substring(data_slice, boundary).unwrap_or(data_slice.len())
    } else {
        data_slice.len()
    };

    if start_pos >= end_pos {
        return;
    }

    match mode {
        0 => {
            // CHARACTERS BY replacement: replace each character in range
            // In CHARACTERS mode, replacement_len must be 1
            if repl_slice.is_empty() {
                return;
            }
            let repl_byte = repl_slice[0];
            for i in start_pos..end_pos {
                data_slice[i] = repl_byte;
            }
        }
        1 => {
            // ALL: replace all occurrences of search by replacement
            if search.is_null() || search_len == 0 {
                return;
            }
            // SAFETY: Caller guarantees `search` points to `search_len` readable bytes.
            let search_slice = std::slice::from_raw_parts(search, search_len as usize);
            let slen = search_slice.len();
            let rlen = repl_slice.len();
            let mut i = start_pos;
            while i + slen <= end_pos {
                if &data_slice[i..i + slen] == search_slice {
                    // Replace: copy replacement bytes (up to search_len)
                    let copy_len = slen.min(rlen);
                    data_slice[i..i + copy_len].copy_from_slice(&repl_slice[..copy_len]);
                    // If replacement is shorter, pad with spaces
                    if rlen < slen {
                        for j in i + rlen..i + slen {
                            data_slice[j] = b' ';
                        }
                    }
                    i += slen;
                } else {
                    i += 1;
                }
            }
        }
        2 => {
            // LEADING: replace leading occurrences only
            if search.is_null() || search_len == 0 {
                return;
            }
            // SAFETY: Caller guarantees `search` points to `search_len` readable bytes.
            let search_slice = std::slice::from_raw_parts(search, search_len as usize);
            let slen = search_slice.len();
            let rlen = repl_slice.len();
            let mut i = start_pos;
            while i + slen <= end_pos {
                if &data_slice[i..i + slen] == search_slice {
                    let copy_len = slen.min(rlen);
                    data_slice[i..i + copy_len].copy_from_slice(&repl_slice[..copy_len]);
                    if rlen < slen {
                        for j in i + rlen..i + slen {
                            data_slice[j] = b' ';
                        }
                    }
                    i += slen;
                } else {
                    break; // stop at first non-match (LEADING)
                }
            }
        }
        3 => {
            // FIRST: replace only the first occurrence
            if search.is_null() || search_len == 0 {
                return;
            }
            // SAFETY: Caller guarantees `search` points to `search_len` readable bytes.
            let search_slice = std::slice::from_raw_parts(search, search_len as usize);
            let slen = search_slice.len();
            let rlen = repl_slice.len();
            let effective = &data_slice[start_pos..end_pos];
            if let Some(pos) = find_substring(effective, search_slice) {
                let abs_pos = start_pos + pos;
                let copy_len = slen.min(rlen);
                data_slice[abs_pos..abs_pos + copy_len].copy_from_slice(&repl_slice[..copy_len]);
                if rlen < slen {
                    for j in abs_pos + rlen..abs_pos + slen {
                        data_slice[j] = b' ';
                    }
                }
            }
        }
        _ => {}
    }
}

/// INSPECT ... CONVERTING from_chars BY to_chars
///   [BEFORE|AFTER INITIAL boundary]
///
/// # Safety
///
/// - `data` must point to at least `data_len` writable bytes, or be null.
/// - `from_chars` must point to at least `from_len` readable bytes, or be null.
/// - `to_chars` must point to at least `to_len` readable bytes, or be null.
/// - `before_initial` must point to at least `before_initial_len` readable bytes, or be null.
/// - `after_initial` must point to at least `after_initial_len` readable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_inspect_converting(
    data: *mut u8,
    data_len: u32,
    from_chars: *const u8,
    from_len: u32,
    to_chars: *const u8,
    to_len: u32,
    before_initial: *const u8,
    before_initial_len: u32,
    after_initial: *const u8,
    after_initial_len: u32,
) {
    if data.is_null() || from_chars.is_null() || to_chars.is_null() {
        return;
    }

    // SAFETY: Caller guarantees all pointers are valid for their respective lengths.
    let data_slice = std::slice::from_raw_parts_mut(data, data_len as usize);
    let from_slice = std::slice::from_raw_parts(from_chars, from_len as usize);
    let to_slice = std::slice::from_raw_parts(to_chars, to_len as usize);

    // Determine effective range
    let start_pos = if !after_initial.is_null() && after_initial_len > 0 {
        // SAFETY: Caller guarantees `after_initial` points to `after_initial_len` readable bytes.
        let boundary = std::slice::from_raw_parts(after_initial, after_initial_len as usize);
        find_substring(data_slice, boundary)
            .map(|pos| pos + boundary.len())
            .unwrap_or(data_slice.len())
    } else {
        0
    };

    let end_pos = if !before_initial.is_null() && before_initial_len > 0 {
        // SAFETY: Caller guarantees `before_initial` points to `before_initial_len` readable bytes.
        let boundary = std::slice::from_raw_parts(before_initial, before_initial_len as usize);
        find_substring(data_slice, boundary).unwrap_or(data_slice.len())
    } else {
        data_slice.len()
    };

    // Build translation table from "from" and "to" characters
    // Each character in "from" maps to the corresponding character in "to"
    for i in start_pos..end_pos {
        if let Some(idx) = from_slice.iter().position(|&c| c == data_slice[i]) {
            if idx < to_slice.len() {
                data_slice[i] = to_slice[idx];
            }
        }
    }
}

/// UNSTRING source DELIMITED BY delimiter INTO targets...
///
/// Simplified runtime: handles a single delimiter and multiple targets.
///
/// Returns 1 if overflow occurred (more fields than targets), 0 otherwise.
///
/// # Safety
///
/// - `source` must point to at least `source_len` readable bytes, or be null.
/// - `delimiter` must point to at least `delimiter_len` readable bytes, or be null.
/// - `targets` must point to an array of `num_targets` pointers, or be null.
/// - `target_lens` must point to an array of `num_targets` u32 values, or be null.
/// - Each non-null pointer in `targets` must point to at least the corresponding `target_lens` writable bytes.
/// - `pointer` must point to at least `pointer_len` writable bytes, or be null.
/// - `tally` must point to at least `tally_len` writable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_unstring(
    source: *const u8,
    source_len: u32,
    delimiter: *const u8,
    delimiter_len: u32,
    all_delim: u32,
    targets: *const *mut u8,
    target_lens: *const u32,
    num_targets: u32,
    pointer: *mut u8,
    pointer_len: u32,
    tally: *mut u8,
    tally_len: u32,
) -> i32 {
    if source.is_null() || targets.is_null() || target_lens.is_null() {
        return 0;
    }

    // SAFETY: Caller guarantees all pointers are valid for their respective lengths.
    let source_slice = std::slice::from_raw_parts(source, source_len as usize);
    let target_ptrs = std::slice::from_raw_parts(targets, num_targets as usize);
    let target_lengths = std::slice::from_raw_parts(target_lens, num_targets as usize);

    // Determine starting position (1-based from POINTER)
    let start_pos = if !pointer.is_null() && pointer_len > 0 {
        // SAFETY: Caller guarantees `pointer` points to `pointer_len` readable bytes.
        let ptr_slice = std::slice::from_raw_parts(pointer, pointer_len as usize);
        let val = read_display_numeric(ptr_slice);
        if val < 1 {
            0usize
        } else {
            (val - 1) as usize
        }
    } else {
        0
    };

    if start_pos >= source_slice.len() {
        return 1; // overflow: pointer beyond source
    }

    let effective = &source_slice[start_pos..];

    let delim = if !delimiter.is_null() && delimiter_len > 0 {
        // SAFETY: Caller guarantees `delimiter` points to `delimiter_len` readable bytes.
        Some(std::slice::from_raw_parts(delimiter, delimiter_len as usize))
    } else {
        None
    };

    // Split the effective source by delimiter
    let mut target_idx = 0u32;
    let mut pos = 0usize;
    let mut fields_found = 0u32;
    let mut overflow = false;

    loop {
        if pos >= effective.len() {
            break;
        }

        // Find the next delimiter
        let field_end = if let Some(d) = delim {
            find_substring(&effective[pos..], d)
                .map(|p| pos + p)
                .unwrap_or(effective.len())
        } else {
            effective.len()
        };

        // Extract the field
        let field = &effective[pos..field_end];

        if target_idx < num_targets {
            // Copy field into target buffer
            let tgt_ptr = target_ptrs[target_idx as usize];
            let tgt_len = target_lengths[target_idx as usize] as usize;

            if !tgt_ptr.is_null() {
                // SAFETY: Caller guarantees each target pointer is valid for its length.
                let tgt_slice = std::slice::from_raw_parts_mut(tgt_ptr, tgt_len);
                // Space-fill first
                for b in tgt_slice.iter_mut() {
                    *b = b' ';
                }
                // Copy field data
                let copy_len = field.len().min(tgt_len);
                tgt_slice[..copy_len].copy_from_slice(&field[..copy_len]);
            }

            target_idx += 1;
            fields_found += 1;
        } else {
            overflow = true;
            break;
        }

        // Advance past the delimiter
        if field_end < effective.len() {
            if let Some(d) = delim {
                pos = field_end + d.len();
                // If ALL, skip consecutive delimiters
                if all_delim != 0 {
                    while pos + d.len() <= effective.len() && &effective[pos..pos + d.len()] == d {
                        pos += d.len();
                    }
                }
            } else {
                pos = field_end;
            }
        } else {
            break;
        }
    }

    // Update POINTER: set to start_pos + pos + 1 (1-based)
    if !pointer.is_null() && pointer_len > 0 {
        let new_pos = (start_pos + pos + 1) as i64;
        // SAFETY: Caller guarantees `pointer` points to `pointer_len` writable bytes.
        let ptr_slice = std::slice::from_raw_parts_mut(pointer, pointer_len as usize);
        write_display_numeric(ptr_slice, new_pos);
    }

    // Update TALLYING: add fields_found to current tally value
    if !tally.is_null() && tally_len > 0 {
        // SAFETY: Caller guarantees `tally` points to `tally_len` writable bytes.
        let tally_slice = std::slice::from_raw_parts_mut(tally, tally_len as usize);
        let current = read_display_numeric(tally_slice);
        write_display_numeric(tally_slice, current + fields_found as i64);
    }

    if overflow {
        1
    } else {
        0
    }
}

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

/// Find the first occurrence of `needle` in `haystack`.
fn find_substring(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    if needle.is_empty() || needle.len() > haystack.len() {
        return None;
    }
    for i in 0..=(haystack.len() - needle.len()) {
        if &haystack[i..i + needle.len()] == needle {
            return Some(i);
        }
    }
    None
}

/// Count all non-overlapping occurrences of `needle` in `haystack`.
fn count_all_occurrences(haystack: &[u8], needle: &[u8]) -> i64 {
    if needle.is_empty() || needle.len() > haystack.len() {
        return 0;
    }
    let mut count = 0i64;
    let mut i = 0;
    while i + needle.len() <= haystack.len() {
        if &haystack[i..i + needle.len()] == needle {
            count += 1;
            i += needle.len();
        } else {
            i += 1;
        }
    }
    count
}

/// Count leading (consecutive from the start) occurrences of `needle`.
fn count_leading_occurrences(haystack: &[u8], needle: &[u8]) -> i64 {
    if needle.is_empty() || needle.len() > haystack.len() {
        return 0;
    }
    let mut count = 0i64;
    let mut i = 0;
    while i + needle.len() <= haystack.len() {
        if &haystack[i..i + needle.len()] == needle {
            count += 1;
            i += needle.len();
        } else {
            break;
        }
    }
    count
}

/// Read a display-format numeric value (ASCII digits) as an i64.
fn read_display_numeric(data: &[u8]) -> i64 {
    let mut result: i64 = 0;
    for &b in data {
        if b.is_ascii_digit() {
            result = result * 10 + (b - b'0') as i64;
        }
    }
    result
}

/// Write an i64 value as display-format numeric (right-justified, zero-padded).
fn write_display_numeric(data: &mut [u8], value: i64) {
    let mut val = if value < 0 { -value } else { value };
    for b in data.iter_mut() {
        *b = b'0';
    }
    for i in (0..data.len()).rev() {
        data[i] = b'0' + (val % 10) as u8;
        val /= 10;
        if val == 0 {
            break;
        }
    }
}

/// Per-field UNSTRING: extracts one field from source starting at the position
/// given by the POINTER field, and advances the POINTER past the delimiter.
///
/// # Safety
///
/// - `source` must point to at least `source_len` readable bytes, or be null.
/// - `delimiter` must point to at least `delimiter_len` readable bytes, or be null.
/// - `target` must point to at least `target_len` writable bytes, or be null.
/// - `pointer` must point to at least `pointer_len` writable bytes, or be null.
/// - `tally` must point to at least `tally_len` writable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_unstring_field(
    source: *const u8,
    source_len: u32,
    delimiter: *const u8,
    delimiter_len: u32,
    all_delim: u32,
    target: *mut u8,
    target_len: u32,
    pointer: *mut u8,
    pointer_len: u32,
    tally: *mut u8,
    tally_len: u32,
) {
    if source.is_null() || target.is_null() {
        return;
    }

    // SAFETY: Caller guarantees `source` points to `source_len` readable bytes.
    let source_slice = std::slice::from_raw_parts(source, source_len as usize);
    // SAFETY: Caller guarantees `target` points to `target_len` writable bytes.
    let target_slice = std::slice::from_raw_parts_mut(target, target_len as usize);

    // Read pointer value (1-based)
    let start_pos = if !pointer.is_null() && pointer_len > 0 {
        // SAFETY: Caller guarantees `pointer` points to `pointer_len` readable bytes.
        let ptr_slice = std::slice::from_raw_parts(pointer, pointer_len as usize);
        let val = read_display_numeric(ptr_slice);
        if val < 1 {
            0usize
        } else {
            (val - 1) as usize
        }
    } else {
        0
    };

    if start_pos >= source_slice.len() {
        // Overflow: space-fill target
        for b in target_slice.iter_mut() {
            *b = b' ';
        }
        return;
    }

    let remaining = &source_slice[start_pos..];

    // Find delimiter
    let delim = if !delimiter.is_null() && delimiter_len > 0 {
        // SAFETY: Caller guarantees `delimiter` points to `delimiter_len` readable bytes.
        Some(std::slice::from_raw_parts(delimiter, delimiter_len as usize))
    } else {
        None
    };

    let (field_end, delim_skip) = if let Some(d) = delim {
        match find_substring(remaining, d) {
            Some(pos) => {
                let mut skip = d.len();
                // If ALL, skip consecutive delimiters
                if all_delim != 0 {
                    let mut next = pos + d.len();
                    while next + d.len() <= remaining.len() && &remaining[next..next + d.len()] == d
                    {
                        next += d.len();
                        skip += d.len();
                    }
                }
                (pos, skip)
            }
            None => (remaining.len(), 0),
        }
    } else {
        (remaining.len(), 0)
    };

    // Copy field into target, space-padded
    let field = &remaining[..field_end];
    for b in target_slice.iter_mut() {
        *b = b' ';
    }
    let copy_len = field.len().min(target_slice.len());
    target_slice[..copy_len].copy_from_slice(&field[..copy_len]);

    // Update pointer
    if !pointer.is_null() && pointer_len > 0 {
        let new_pos = (start_pos + field_end + delim_skip + 1) as i64;
        // SAFETY: Caller guarantees `pointer` points to `pointer_len` writable bytes.
        let ptr_slice = std::slice::from_raw_parts_mut(pointer, pointer_len as usize);
        write_display_numeric(ptr_slice, new_pos);
    }

    // Update tally
    if !tally.is_null() && tally_len > 0 {
        // SAFETY: Caller guarantees `tally` points to `tally_len` writable bytes.
        let tally_slice = std::slice::from_raw_parts_mut(tally, tally_len as usize);
        let current = read_display_numeric(tally_slice);
        write_display_numeric(tally_slice, current + 1);
    }
}

/// Per-field UNSTRING with multiple OR delimiters.
/// Finds the earliest matching delimiter among up to 3 alternatives.
///
/// # Safety
///
/// All pointer/length pairs must be valid. Unused delimiter slots must have
/// null pointers and zero lengths.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_unstring_field_or(
    source: *const u8,
    source_len: u32,
    num_delimiters: u32,
    delim1: *const u8,
    delim1_len: u32,
    delim1_all: u32,
    delim2: *const u8,
    delim2_len: u32,
    delim2_all: u32,
    delim3: *const u8,
    delim3_len: u32,
    delim3_all: u32,
    target: *mut u8,
    target_len: u32,
    pointer: *mut u8,
    pointer_len: u32,
    tally: *mut u8,
    tally_len: u32,
    count: *mut u8,
    count_len: u32,
    delim_in: *mut u8,
    delim_in_len: u32,
) {
    if source.is_null() || target.is_null() {
        return;
    }

    let source_slice = std::slice::from_raw_parts(source, source_len as usize);
    let target_slice = std::slice::from_raw_parts_mut(target, target_len as usize);

    // Read pointer value (1-based)
    let start_pos = if !pointer.is_null() && pointer_len > 0 {
        let ptr_slice = std::slice::from_raw_parts(pointer, pointer_len as usize);
        let val = read_display_numeric(ptr_slice);
        if val < 1 { 0usize } else { (val - 1) as usize }
    } else {
        0
    };

    if start_pos >= source_slice.len() {
        for b in target_slice.iter_mut() {
            *b = b' ';
        }
        return;
    }

    let remaining = &source_slice[start_pos..];

    // Build delimiter list from the up-to-3 slots
    struct DelimInfo<'a> {
        bytes: &'a [u8],
        all: bool,
    }
    let mut delims: Vec<DelimInfo> = Vec::new();
    let slots: [(* const u8, u32, u32); 3] = [
        (delim1, delim1_len, delim1_all),
        (delim2, delim2_len, delim2_all),
        (delim3, delim3_len, delim3_all),
    ];
    for i in 0..(num_delimiters as usize).min(3) {
        let (ptr, len, all_flag) = slots[i];
        if !ptr.is_null() && len > 0 {
            delims.push(DelimInfo {
                bytes: std::slice::from_raw_parts(ptr, len as usize),
                all: all_flag != 0,
            });
        }
    }

    // Find the earliest delimiter match among all alternatives
    let mut best_pos: Option<usize> = None;
    let mut best_idx: usize = 0;
    for (di, d) in delims.iter().enumerate() {
        if let Some(pos) = find_substring(remaining, d.bytes) {
            if best_pos.is_none() || pos < best_pos.unwrap() {
                best_pos = Some(pos);
                best_idx = di;
            }
        }
    }

    let (field_end, delim_skip, matched_delim) = if let Some(pos) = best_pos {
        let d = &delims[best_idx];
        let mut skip = d.bytes.len();
        // If ALL, skip consecutive occurrences of the matched delimiter
        if d.all {
            let mut next = pos + d.bytes.len();
            while next + d.bytes.len() <= remaining.len()
                && &remaining[next..next + d.bytes.len()] == d.bytes
            {
                next += d.bytes.len();
                skip += d.bytes.len();
            }
        }
        (pos, skip, Some(d.bytes))
    } else {
        (remaining.len(), 0, None)
    };

    // Copy field into target, space-padded
    let field = &remaining[..field_end];
    for b in target_slice.iter_mut() {
        *b = b' ';
    }
    let copy_len = field.len().min(target_slice.len());
    target_slice[..copy_len].copy_from_slice(&field[..copy_len]);

    // Update DELIMITER IN field
    if !delim_in.is_null() && delim_in_len > 0 {
        let di_slice = std::slice::from_raw_parts_mut(delim_in, delim_in_len as usize);
        for b in di_slice.iter_mut() {
            *b = b' ';
        }
        if let Some(md) = matched_delim {
            let copy = md.len().min(di_slice.len());
            di_slice[..copy].copy_from_slice(&md[..copy]);
        }
    }

    // Update COUNT IN field
    if !count.is_null() && count_len > 0 {
        let count_slice = std::slice::from_raw_parts_mut(count, count_len as usize);
        write_display_numeric(count_slice, field.len() as i64);
    }

    // Update pointer
    if !pointer.is_null() && pointer_len > 0 {
        let new_pos = (start_pos + field_end + delim_skip + 1) as i64;
        let ptr_slice = std::slice::from_raw_parts_mut(pointer, pointer_len as usize);
        write_display_numeric(ptr_slice, new_pos);
    }

    // Update tally
    if !tally.is_null() && tally_len > 0 {
        let tally_slice = std::slice::from_raw_parts_mut(tally, tally_len as usize);
        let current = read_display_numeric(tally_slice);
        write_display_numeric(tally_slice, current + 1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tallying_all() {
        let data = b"HELLO WORLD";
        let mut tally = *b"00";
        let search = b"L";
        // SAFETY: All pointers are valid for their respective lengths.
        unsafe {
            cobolrt_inspect_tallying(
                data.as_ptr(), 11,
                tally.as_mut_ptr(), 2,
                1, // ALL
                search.as_ptr(), 1,
                std::ptr::null(), 0,
                std::ptr::null(), 0,
            );
        }
        assert_eq!(&tally, b"03");
    }

    #[test]
    fn test_tallying_leading() {
        let data = b"AAABCD";
        let mut tally = *b"00";
        let search = b"A";
        // SAFETY: All pointers are valid for their respective lengths.
        unsafe {
            cobolrt_inspect_tallying(
                data.as_ptr(), 6,
                tally.as_mut_ptr(), 2,
                2, // LEADING
                search.as_ptr(), 1,
                std::ptr::null(), 0,
                std::ptr::null(), 0,
            );
        }
        assert_eq!(&tally, b"03");
    }

    #[test]
    fn test_tallying_characters() {
        let data = b"HELLO";
        let mut tally = *b"00";
        // SAFETY: All pointers are valid for their respective lengths.
        unsafe {
            cobolrt_inspect_tallying(
                data.as_ptr(), 5,
                tally.as_mut_ptr(), 2,
                0, // CHARACTERS
                std::ptr::null(), 0,
                std::ptr::null(), 0,
                std::ptr::null(), 0,
            );
        }
        assert_eq!(&tally, b"05");
    }

    #[test]
    fn test_replacing_all() {
        let mut data = *b"HELLO WORLD";
        let search = b"L";
        let replacement = b"X";
        // SAFETY: All pointers are valid for their respective lengths.
        unsafe {
            cobolrt_inspect_replacing(
                data.as_mut_ptr(), 11,
                1, // ALL
                search.as_ptr(), 1,
                replacement.as_ptr(), 1,
                std::ptr::null(), 0,
                std::ptr::null(), 0,
            );
        }
        assert_eq!(&data, b"HEXXO WORXD");
    }

    #[test]
    fn test_replacing_first() {
        let mut data = *b"HELLO WORLD";
        let search = b"L";
        let replacement = b"X";
        // SAFETY: All pointers are valid for their respective lengths.
        unsafe {
            cobolrt_inspect_replacing(
                data.as_mut_ptr(), 11,
                3, // FIRST
                search.as_ptr(), 1,
                replacement.as_ptr(), 1,
                std::ptr::null(), 0,
                std::ptr::null(), 0,
            );
        }
        assert_eq!(&data, b"HEXLO WORLD");
    }

    #[test]
    fn test_converting() {
        let mut data = *b"HELLO";
        let from = b"HELO";
        let to = b"helo";
        // SAFETY: All pointers are valid for their respective lengths.
        unsafe {
            cobolrt_inspect_converting(
                data.as_mut_ptr(), 5,
                from.as_ptr(), 4,
                to.as_ptr(), 4,
                std::ptr::null(), 0,
                std::ptr::null(), 0,
            );
        }
        assert_eq!(&data, b"hello");
    }

    #[test]
    fn test_unstring_basic() {
        let source = b"ONE,TWO,THREE";
        let delimiter = b",";
        let mut target1 = [b' '; 10];
        let mut target2 = [b' '; 10];
        let mut target3 = [b' '; 10];

        let targets: [*mut u8; 3] = [
            target1.as_mut_ptr(),
            target2.as_mut_ptr(),
            target3.as_mut_ptr(),
        ];
        let target_lens: [u32; 3] = [10, 10, 10];

        // SAFETY: All pointers are valid for their respective lengths.
        let result = unsafe {
            cobolrt_unstring(
                source.as_ptr(), 13,
                delimiter.as_ptr(), 1,
                0, // not ALL
                targets.as_ptr(), target_lens.as_ptr(), 3,
                std::ptr::null_mut(), 0,
                std::ptr::null_mut(), 0,
            )
        };

        assert_eq!(result, 0);
        assert_eq!(&target1[..3], b"ONE");
        assert_eq!(&target2[..3], b"TWO");
        assert_eq!(&target3[..5], b"THREE");
    }

    #[test]
    fn test_tallying_with_before_initial() {
        let data = b"HELLO WORLD";
        let mut tally = *b"00";
        let search = b"L";
        let before = b" ";
        // SAFETY: All pointers are valid for their respective lengths.
        unsafe {
            cobolrt_inspect_tallying(
                data.as_ptr(), 11,
                tally.as_mut_ptr(), 2,
                1, // ALL
                search.as_ptr(), 1,
                before.as_ptr(), 1, // BEFORE INITIAL " "
                std::ptr::null(), 0,
            );
        }
        assert_eq!(&tally, b"02"); // only "L" in "HELLO" (before " ")
    }
}
