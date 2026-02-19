/// Allocate memory for COBOL data items (WORKING-STORAGE)
#[no_mangle]
pub extern "C" fn cobolrt_alloc(size: u32) -> *mut u8 {
    let layout =
        std::alloc::Layout::from_size_align(size as usize, 8).expect("invalid allocation size");
    unsafe {
        let ptr = std::alloc::alloc_zeroed(layout);
        if ptr.is_null() {
            std::alloc::handle_alloc_error(layout);
        }
        ptr
    }
}

/// Free memory allocated by cobolrt_alloc
#[no_mangle]
pub extern "C" fn cobolrt_free(ptr: *mut u8, size: u32) {
    if ptr.is_null() {
        return;
    }
    let layout =
        std::alloc::Layout::from_size_align(size as usize, 8).expect("invalid allocation size");
    unsafe {
        std::alloc::dealloc(ptr, layout);
    }
}

/// Move (copy) data between fields, with space/zero padding
/// Alphanumeric: left-justified, space-padded
/// Numeric: right-justified, zero-padded
#[no_mangle]
pub extern "C" fn cobolrt_move_alphanumeric(
    src: *const u8,
    src_len: u32,
    dest: *mut u8,
    dest_len: u32,
) {
    if src.is_null() || dest.is_null() {
        return;
    }
    let copy_len = std::cmp::min(src_len, dest_len) as usize;
    unsafe {
        std::ptr::copy_nonoverlapping(src, dest, copy_len);
        // Pad with spaces
        if (dest_len as usize) > copy_len {
            std::ptr::write_bytes(dest.add(copy_len), b' ', dest_len as usize - copy_len);
        }
    }
}

/// Move numeric data with proper alignment and zero-padding.
/// Handles different PIC sizes and scales by aligning on the decimal point.
///
/// `src_scale` / `dest_scale` are the number of implied decimal digits.
/// `src_dot_pos` / `dest_dot_pos` are the byte position of a literal '.'
/// character in the display buffer (-1 if no literal dot).
#[no_mangle]
pub extern "C" fn cobolrt_move_numeric(
    src: *const u8,
    src_len: u32,
    src_scale: i32,
    _src_dot_pos: i32,
    dest: *mut u8,
    dest_len: u32,
    dest_scale: i32,
    dest_dot_pos: i32,
    rounded: i32,
) {
    if src.is_null() || dest.is_null() {
        return;
    }
    let src_slice = unsafe { std::slice::from_raw_parts(src, src_len as usize) };

    // Extract the integer value from source, skipping any '.' character
    let mut src_val: i64 = 0;
    for &b in src_slice {
        if b >= b'0' && b <= b'9' {
            src_val = src_val * 10 + (b - b'0') as i64;
        }
    }

    // Adjust scale: if dest has more decimal places than src, multiply;
    // if fewer, divide (truncating or rounding)
    let scale_diff = dest_scale - src_scale;
    let mut adjusted = src_val;
    if scale_diff > 0 {
        for _ in 0..scale_diff {
            adjusted *= 10;
        }
    } else if scale_diff < 0 {
        if rounded != 0 {
            // Round: truncate all but the last digit, check the last, then finish
            for _ in 0..((-scale_diff) - 1) {
                adjusted /= 10;
            }
            let remainder = adjusted % 10;
            adjusted /= 10;
            if remainder >= 5 {
                adjusted += 1;
            }
        } else {
            for _ in 0..(-scale_diff) {
                adjusted /= 10;
            }
        }
    }

    // Write the result into the destination buffer
    let dest_slice = unsafe { std::slice::from_raw_parts_mut(dest, dest_len as usize) };

    // Fill with '0' first
    for b in dest_slice.iter_mut() {
        *b = b'0';
    }

    // Place the dot if needed
    if dest_dot_pos >= 0 && (dest_dot_pos as usize) < dest_slice.len() {
        dest_slice[dest_dot_pos as usize] = b'.';
    }

    // Write digits right-to-left, skipping the dot position
    if adjusted < 0 {
        adjusted = -adjusted;
    }
    let mut val = adjusted as u64;
    for i in (0..dest_len as usize).rev() {
        if dest_dot_pos >= 0 && i == dest_dot_pos as usize {
            continue;
        }
        if val > 0 {
            dest_slice[i] = b'0' + (val % 10) as u8;
            val /= 10;
        }
    }
}

/// Move numeric display data to an alphanumeric field.
/// Left-justifies the numeric representation and space-pads the right.
#[no_mangle]
pub extern "C" fn cobolrt_move_num_to_alpha(
    src: *const u8,
    src_len: u32,
    dest: *mut u8,
    dest_len: u32,
) {
    if src.is_null() || dest.is_null() {
        return;
    }
    let copy_len = std::cmp::min(src_len, dest_len) as usize;
    unsafe {
        std::ptr::copy_nonoverlapping(src, dest, copy_len);
        if (dest_len as usize) > copy_len {
            std::ptr::write_bytes(dest.add(copy_len), b' ', dest_len as usize - copy_len);
        }
    }
}

/// Move alphanumeric data to a numeric field.
/// Parses digits from the alphanumeric source, ignoring non-digit characters.
/// Result is right-justified and zero-padded.
#[no_mangle]
pub extern "C" fn cobolrt_move_alpha_to_num(
    src: *const u8,
    src_len: u32,
    dest: *mut u8,
    dest_len: u32,
    dest_dot_pos: i32,
) {
    if src.is_null() || dest.is_null() {
        return;
    }
    let src_slice = unsafe { std::slice::from_raw_parts(src, src_len as usize) };
    let dest_slice = unsafe { std::slice::from_raw_parts_mut(dest, dest_len as usize) };

    // Parse the source as a number
    let mut int_part: i64 = 0;
    let mut frac_part: i64 = 0;
    let mut frac_digits: i32 = 0;
    let mut in_frac = false;

    for &b in src_slice {
        if b == b'.' {
            in_frac = true;
            continue;
        }
        if b >= b'0' && b <= b'9' {
            if in_frac {
                frac_part = frac_part * 10 + (b - b'0') as i64;
                frac_digits += 1;
            } else {
                int_part = int_part * 10 + (b - b'0') as i64;
            }
        }
    }

    // Compute the number of fractional digits in dest
    let dest_frac_digits = if dest_dot_pos >= 0 {
        dest_len as i32 - dest_dot_pos - 1
    } else {
        0
    };

    // Build the full integer representation
    let mut result = int_part;
    if dest_frac_digits > 0 {
        for _ in 0..dest_frac_digits {
            result *= 10;
        }
        let mut frac_adj = frac_part;
        if frac_digits < dest_frac_digits {
            for _ in 0..(dest_frac_digits - frac_digits) {
                frac_adj *= 10;
            }
        } else if frac_digits > dest_frac_digits {
            for _ in 0..(frac_digits - dest_frac_digits) {
                frac_adj /= 10;
            }
        }
        result += frac_adj;
    }

    // Fill with '0'
    for b in dest_slice.iter_mut() {
        *b = b'0';
    }
    if dest_dot_pos >= 0 && (dest_dot_pos as usize) < dest_slice.len() {
        dest_slice[dest_dot_pos as usize] = b'.';
    }

    // Write digits right-to-left, skipping dot position
    let mut val = result as u64;
    for i in (0..dest_len as usize).rev() {
        if dest_dot_pos >= 0 && i == dest_dot_pos as usize {
            continue;
        }
        if val > 0 {
            dest_slice[i] = b'0' + (val % 10) as u8;
            val /= 10;
        }
    }
}

/// Initialize a data item (INITIALIZE verb)
#[no_mangle]
pub extern "C" fn cobolrt_initialize_alphanumeric(data: *mut u8, len: u32) {
    if data.is_null() {
        return;
    }
    unsafe {
        std::ptr::write_bytes(data, b' ', len as usize);
    }
}

#[no_mangle]
pub extern "C" fn cobolrt_initialize_numeric(data: *mut u8, len: u32) {
    if data.is_null() {
        return;
    }
    unsafe {
        std::ptr::write_bytes(data, b'0', len as usize);
    }
}
