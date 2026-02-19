//! Runtime arithmetic functions for COBOL display-format numeric values.
//!
//! Display format means ASCII digit strings like "00123" or "12.50".
//! These functions parse display-format strings into scaled integer
//! representations, perform arithmetic, and write results back to
//! display-format buffers.

use std::cell::RefCell;

// ---------------------------------------------------------------------------
// Thread-local result buffer used by functions that return a pointer
// (cobolrt_decimal_* and cobolrt_negate_numeric).
// ---------------------------------------------------------------------------
thread_local! {
    static RESULT_BUF: RefCell<Vec<u8>> = RefCell::new(vec![b'0'; 64]);
    static NEGATE_BUF: RefCell<Vec<u8>> = RefCell::new(vec![b'0'; 64]);
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Parsed representation of a display-format numeric value.
/// `digits` is the absolute integer with all decimal digits concatenated.
/// `scale` is the number of fractional digits (digits after the decimal
/// point). `negative` tracks the sign.
struct ParsedNumeric {
    digits: i128,
    scale: u32,
    negative: bool,
}

/// Parse a display-format numeric byte slice into its components.
/// Handles: leading/trailing sign characters ('-', '+'), embedded decimal
/// points, leading zeros, and non-digit characters (which are skipped).
fn parse_display(data: &[u8]) -> ParsedNumeric {
    let mut negative = false;
    let mut value: i128 = 0;
    let mut scale: u32 = 0;
    let mut in_frac = false;

    for &b in data {
        match b {
            b'-' => negative = true,
            b'+' => {}
            b'.' => in_frac = true,
            b'0'..=b'9' => {
                value = value * 10 + (b - b'0') as i128;
                if in_frac {
                    scale += 1;
                }
            }
            _ => {} // skip spaces and other characters
        }
    }

    ParsedNumeric {
        digits: value,
        scale,
        negative,
    }
}

/// Align two parsed numerics to the same scale by multiplying the one
/// with fewer fractional digits. Returns (left_val, right_val, common_scale)
/// where both values are signed and scaled to the same number of fractional digits.
fn align_scales(left: &ParsedNumeric, right: &ParsedNumeric) -> (i128, i128, u32) {
    let common_scale = left.scale.max(right.scale);

    let mut l = left.digits;
    for _ in 0..(common_scale - left.scale) {
        l *= 10;
    }
    if left.negative {
        l = -l;
    }

    let mut r = right.digits;
    for _ in 0..(common_scale - right.scale) {
        r *= 10;
    }
    if right.negative {
        r = -r;
    }

    (l, r, common_scale)
}

/// Write a signed scaled-integer value into a display-format destination buffer.
/// `value` is the signed result. `scale` is the number of fractional digits
/// in `value`. The destination buffer `dest` is zero-filled, with a decimal
/// point inserted if the buffer contains one already or if `scale > 0`.
///
/// The function scans the existing destination to see if it contains a '.'
/// and uses that position. Otherwise, if scale > 0, it places digits after
/// an implicit decimal point at (dest_len - scale).
fn write_display(value: i128, scale: u32, dest: &mut [u8]) {
    let dest_len = dest.len();
    if dest_len == 0 {
        return;
    }

    // Detect if destination already has a '.' — preserve that position.
    let existing_dot = dest.iter().position(|&b| b == b'.');

    // Fill destination with '0'
    for b in dest.iter_mut() {
        *b = b'0';
    }

    let abs_value = value.unsigned_abs();

    if let Some(dot_pos) = existing_dot {
        // Destination has an explicit decimal point.
        dest[dot_pos] = b'.';

        // Number of digit positions after the dot.
        let dest_frac = dest_len - dot_pos - 1;
        // Number of digit positions before the dot.
        let dest_int = dot_pos;

        // We need to adjust the value so its scale matches dest_frac.
        let adjusted = if (scale as usize) < dest_frac {
            let mut v = abs_value;
            for _ in 0..(dest_frac - scale as usize) {
                v *= 10;
            }
            v
        } else if (scale as usize) > dest_frac {
            let mut v = abs_value;
            for _ in 0..(scale as usize - dest_frac) {
                v /= 10;
            }
            v
        } else {
            abs_value
        };

        // Write fractional digits right-to-left from end of buffer.
        let mut val = adjusted;
        for i in (dot_pos + 1..dest_len).rev() {
            dest[i] = b'0' + (val % 10) as u8;
            val /= 10;
        }
        // Write integer digits right-to-left before the dot.
        for i in (0..dest_int).rev() {
            dest[i] = b'0' + (val % 10) as u8;
            val /= 10;
        }
    } else if scale > 0 {
        // No explicit dot in destination. Write all digits as a flat integer
        // after adjusting for scale: the destination is treated as an integer
        // field, so we remove fractional digits by dividing.
        let mut adjusted = abs_value;
        for _ in 0..scale {
            adjusted /= 10;
        }
        let mut val = adjusted;
        for i in (0..dest_len).rev() {
            dest[i] = b'0' + (val % 10) as u8;
            val /= 10;
        }
    } else {
        // No decimal point, no scale. Write integer digits right-to-left.
        let mut val = abs_value;
        for i in (0..dest_len).rev() {
            dest[i] = b'0' + (val % 10) as u8;
            val /= 10;
        }
    }
}

/// Write a scaled value into a thread-local result buffer and return a
/// pointer to it. The buffer is resized to `result_len` bytes.
fn write_to_thread_local(value: i128, scale: u32, result_len: u32) -> *mut u8 {
    RESULT_BUF.with(|buf| {
        let mut buf = buf.borrow_mut();
        let len = result_len as usize;
        buf.resize(len, b'0');

        // If the result has fractional digits and space permits, embed a '.'.
        // Place the dot so that `scale` digit positions follow it.
        if scale > 0 && (scale as usize) < len {
            let dot_pos = len - scale as usize - 1;
            // Pre-set the dot so write_display will detect it.
            buf[dot_pos] = b'.';
        }

        write_display(value, scale, &mut buf[..len]);

        // Handle negative: if the value is negative, prepend a '-' sign
        // by shifting digits right if there is room, or overwrite the
        // leading zero.
        if value < 0 && len > 0 {
            // Find the first non-zero digit position to check for room.
            // We simply overwrite buf[0] with '-' if the leading position
            // is '0' (i.e., there is room).
            if buf[0] == b'0' {
                buf[0] = b'-';
            }
        }

        buf.as_mut_ptr()
    })
}

// ---------------------------------------------------------------------------
// Public extern "C" functions
// ---------------------------------------------------------------------------

/// Add two display-format numeric values, store result in dest.
///
/// Display format means ASCII digit strings like "00123" or "12.50".
///
/// # Safety
///
/// - `left` must point to at least `left_len` readable bytes, or be null.
/// - `right` must point to at least `right_len` readable bytes, or be null.
/// - `dest` must point to at least `dest_len` writable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_add_numeric(
    left: *const u8,
    left_len: u32,
    right: *const u8,
    right_len: u32,
    dest: *mut u8,
    dest_len: u32,
) {
    if left.is_null() || right.is_null() || dest.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `left` points to `left_len` readable bytes.
    let l_slice = std::slice::from_raw_parts(left, left_len as usize);
    // SAFETY: Caller guarantees `right` points to `right_len` readable bytes.
    let r_slice = std::slice::from_raw_parts(right, right_len as usize);
    // SAFETY: Caller guarantees `dest` points to `dest_len` writable bytes.
    let d_slice = std::slice::from_raw_parts_mut(dest, dest_len as usize);

    let l = parse_display(l_slice);
    let r = parse_display(r_slice);

    let (lv, rv, scale) = align_scales(&l, &r);
    let result = lv + rv;

    write_display(result, scale, d_slice);
}

/// Subtract right from left, store result in dest.
///
/// # Safety
///
/// - `left` must point to at least `left_len` readable bytes, or be null.
/// - `right` must point to at least `right_len` readable bytes, or be null.
/// - `dest` must point to at least `dest_len` writable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_subtract_numeric(
    left: *const u8,
    left_len: u32,
    right: *const u8,
    right_len: u32,
    dest: *mut u8,
    dest_len: u32,
) {
    if left.is_null() || right.is_null() || dest.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `left` points to `left_len` readable bytes.
    let l_slice = std::slice::from_raw_parts(left, left_len as usize);
    // SAFETY: Caller guarantees `right` points to `right_len` readable bytes.
    let r_slice = std::slice::from_raw_parts(right, right_len as usize);
    // SAFETY: Caller guarantees `dest` points to `dest_len` writable bytes.
    let d_slice = std::slice::from_raw_parts_mut(dest, dest_len as usize);

    let l = parse_display(l_slice);
    let r = parse_display(r_slice);

    let (lv, rv, scale) = align_scales(&l, &r);
    let result = lv - rv;

    write_display(result, scale, d_slice);
}

/// Multiply left by right, store result in dest.
///
/// # Safety
///
/// - `left` must point to at least `left_len` readable bytes, or be null.
/// - `right` must point to at least `right_len` readable bytes, or be null.
/// - `dest` must point to at least `dest_len` writable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_multiply_numeric(
    left: *const u8,
    left_len: u32,
    right: *const u8,
    right_len: u32,
    dest: *mut u8,
    dest_len: u32,
) {
    if left.is_null() || right.is_null() || dest.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `left` points to `left_len` readable bytes.
    let l_slice = std::slice::from_raw_parts(left, left_len as usize);
    // SAFETY: Caller guarantees `right` points to `right_len` readable bytes.
    let r_slice = std::slice::from_raw_parts(right, right_len as usize);
    // SAFETY: Caller guarantees `dest` points to `dest_len` writable bytes.
    let d_slice = std::slice::from_raw_parts_mut(dest, dest_len as usize);

    let l = parse_display(l_slice);
    let r = parse_display(r_slice);

    // For multiplication the scale of the result is the sum of both scales.
    let mut lv = l.digits as i128;
    if l.negative {
        lv = -lv;
    }
    let mut rv = r.digits as i128;
    if r.negative {
        rv = -rv;
    }
    let result = lv * rv;
    let result_scale = l.scale + r.scale;

    write_display(result, result_scale, d_slice);
}

/// Divide with scale handling.
///
/// The signature matches the Cranelift codegen declaration:
///   (src1, src1_len, src1_scale, src2, src2_len, src2_scale,
///    dest, dest_len, dest_scale, dest_dot_pos, rounded)
///
/// # Safety
///
/// - `src1` must point to at least `src1_len` readable bytes, or be null.
/// - `src2` must point to at least `src2_len` readable bytes, or be null.
/// - `dest` must point to at least `dest_len` writable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_divide_scaled(
    src1: *const u8,
    src1_len: u32,
    src1_scale: i32,
    src2: *const u8,
    src2_len: u32,
    src2_scale: i32,
    dest: *mut u8,
    dest_len: u32,
    dest_scale: i32,
    dest_dot_pos: i32,
    rounded: i32,
) {
    if src1.is_null() || src2.is_null() || dest.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `src1` points to `src1_len` readable bytes.
    let s1 = std::slice::from_raw_parts(src1, src1_len as usize);
    // SAFETY: Caller guarantees `src2` points to `src2_len` readable bytes.
    let s2 = std::slice::from_raw_parts(src2, src2_len as usize);
    // SAFETY: Caller guarantees `dest` points to `dest_len` writable bytes.
    let d = std::slice::from_raw_parts_mut(dest, dest_len as usize);

    // Read raw digit values (skipping '.' and other non-digit chars).
    let dividend = parse_display_skip_dot(s1);
    let divisor = parse_display_skip_dot(s2);

    if divisor == 0 {
        // Division by zero: fill result with zeros.
        write_display_edited(0, d, dest_dot_pos);
        return;
    }

    // Scale adjustment:
    // dividend represents value * 10^src1_scale
    // divisor represents value * 10^src2_scale
    // result should represent value * 10^dest_scale
    // => result = dividend * 10^(dest_scale - src1_scale + src2_scale) / divisor
    let scale_adjust = dest_scale - src1_scale + src2_scale;

    let mut adjusted_dividend = dividend;
    if scale_adjust > 0 {
        for _ in 0..scale_adjust {
            adjusted_dividend *= 10;
        }
    } else if scale_adjust < 0 {
        for _ in 0..(-scale_adjust) {
            adjusted_dividend /= 10;
        }
    }

    let result = if rounded != 0 {
        // For rounding, compute one extra decimal place.
        let extended = adjusted_dividend * 10 / divisor;
        let mut r = extended / 10;
        if (extended % 10).abs() >= 5 {
            if extended >= 0 {
                r += 1;
            } else {
                r -= 1;
            }
        }
        r
    } else {
        adjusted_dividend / divisor
    };

    write_display_edited(result, d, dest_dot_pos);
}

/// Parse display digits, skipping non-digit characters (like '.').
/// Returns a signed i128.
fn parse_display_skip_dot(data: &[u8]) -> i128 {
    let mut negative = false;
    let mut result: i128 = 0;
    for &b in data {
        match b {
            b'-' => negative = true,
            b'0'..=b'9' => {
                result = result * 10 + (b - b'0') as i128;
            }
            _ => {}
        }
    }
    if negative {
        -result
    } else {
        result
    }
}

/// Write digits into a display buffer with an optional dot position.
/// Mirrors the C `int_to_display_edited` from the driver.
fn write_display_edited(value: i128, dest: &mut [u8], dot_pos: i32) {
    let len = dest.len();
    let abs_val = value.unsigned_abs();

    // Fill with '0'.
    for b in dest.iter_mut() {
        *b = b'0';
    }
    if dot_pos >= 0 && (dot_pos as usize) < len {
        dest[dot_pos as usize] = b'.';
    }

    // Write digits right-to-left, skipping the dot position.
    let mut val = abs_val;
    for i in (0..len).rev() {
        if dot_pos >= 0 && i == dot_pos as usize {
            continue;
        }
        if val > 0 {
            dest[i] = b'0' + (val % 10) as u8;
            val /= 10;
        }
    }
}

/// Return a pointer to a negated copy of the display-format number.
/// Uses a thread-local buffer.
///
/// # Safety
///
/// - `src` must point to at least `src_len` readable bytes, or be null.
/// - The returned pointer is only valid until the next call to this function
///   on the same thread.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_negate_numeric(src: *const u8, src_len: u32) -> *mut u8 {
    if src.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: Caller guarantees `src` points to `src_len` readable bytes.
    let slice = std::slice::from_raw_parts(src, src_len as usize);

    let parsed = parse_display(slice);

    // Negate: flip the sign.
    let mut lv = parsed.digits as i128;
    if parsed.negative {
        // Was negative, make positive — no sign prefix needed.
    } else {
        lv = -lv;
    }

    NEGATE_BUF.with(|buf| {
        let mut buf = buf.borrow_mut();
        let len = src_len as usize;
        // Allocate one extra byte for potential sign character.
        let buf_len = len + 1;
        buf.resize(buf_len, b'0');

        // If value is negative, first byte is '-', remaining are digits.
        if lv < 0 {
            buf[0] = b'-';
            let abs_val = (-lv) as u128;

            // Fill digit portion with '0'.
            for b in buf[1..buf_len].iter_mut() {
                *b = b'0';
            }

            // Handle decimal point: replicate the source scale.
            if parsed.scale > 0 && (parsed.scale as usize + 1) < buf_len {
                let dot_pos = buf_len - parsed.scale as usize - 1;
                buf[dot_pos] = b'.';

                // Write digits right-to-left, skipping dot.
                let mut val = abs_val;
                for i in (1..buf_len).rev() {
                    if i == dot_pos {
                        continue;
                    }
                    buf[i] = b'0' + (val % 10) as u8;
                    val /= 10;
                }
            } else {
                // No fractional part.
                let mut val = abs_val;
                for i in (1..buf_len).rev() {
                    buf[i] = b'0' + (val % 10) as u8;
                    val /= 10;
                }
            }
        } else {
            // Positive result — write unsigned digits.
            for b in buf.iter_mut() {
                *b = b'0';
            }

            let abs_val = lv as u128;
            if parsed.scale > 0 && (parsed.scale as usize) < buf_len {
                let dot_pos = buf_len - parsed.scale as usize - 1;
                buf[dot_pos] = b'.';

                let mut val = abs_val;
                for i in (0..buf_len).rev() {
                    if i == dot_pos {
                        continue;
                    }
                    buf[i] = b'0' + (val % 10) as u8;
                    val /= 10;
                }
            } else {
                let mut val = abs_val;
                for i in (0..buf_len).rev() {
                    buf[i] = b'0' + (val % 10) as u8;
                    val /= 10;
                }
            }
        }

        buf.as_mut_ptr()
    })
}

/// Parse a display-format number to a 64-bit integer.
/// Skips decimal points and non-digit characters.
///
/// # Safety
///
/// - `src` must point to at least `src_len` readable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_display_to_int(src: *const u8, src_len: u32) -> i64 {
    if src.is_null() {
        return 0;
    }
    // SAFETY: Caller guarantees `src` points to `src_len` readable bytes.
    let slice = std::slice::from_raw_parts(src, src_len as usize);

    let mut negative = false;
    let mut result: i64 = 0;

    for &b in slice {
        match b {
            b'-' => negative = true,
            b'+' => {}
            b'0'..=b'9' => {
                result = result.wrapping_mul(10).wrapping_add((b - b'0') as i64);
            }
            _ => {} // skip '.', spaces, etc.
        }
    }

    if negative {
        -result
    } else {
        result
    }
}

/// COBOL STRING verb: append src to dest at the position indicated by
/// the pointer variable. The pointer is a display-format numeric field
/// containing a 1-based position. After appending, the pointer is
/// updated to reflect the new position.
///
/// # Safety
///
/// - `src` must point to at least `src_len` readable bytes, or be null.
/// - `dest` must point to at least `dest_len` writable bytes, or be null.
/// - `ptr` must point to at least `ptr_len` writable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_string_append(
    src: *const u8,
    src_len: u32,
    dest: *mut u8,
    dest_len: u32,
    ptr: *mut u8,
    ptr_len: u32,
) {
    if src.is_null() || dest.is_null() || ptr.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `src` points to `src_len` readable bytes.
    let src_slice = std::slice::from_raw_parts(src, src_len as usize);
    // SAFETY: Caller guarantees `dest` points to `dest_len` writable bytes.
    let dest_slice = std::slice::from_raw_parts_mut(dest, dest_len as usize);
    // SAFETY: Caller guarantees `ptr` points to `ptr_len` writable bytes.
    let ptr_slice = std::slice::from_raw_parts_mut(ptr, ptr_len as usize);

    // Read current pointer value (1-based, display-format numeric).
    let pos = cobolrt_display_to_int(ptr_slice.as_ptr(), ptr_len);

    // Convert to 0-based index.
    let mut dest_pos = (pos - 1) as usize;

    // Copy source bytes into destination starting at dest_pos.
    let mut copied: u32 = 0;
    for &b in src_slice {
        if dest_pos >= dest_slice.len() {
            break;
        }
        dest_slice[dest_pos] = b;
        dest_pos += 1;
        copied += 1;
    }

    // Update the pointer value.
    let new_pos = pos + copied as i64;
    int_to_display(new_pos, ptr_slice);
}

/// Write an integer value into a display-format buffer (right-justified,
/// zero-padded).
fn int_to_display(value: i64, dest: &mut [u8]) {
    let len = dest.len();
    if len == 0 {
        return;
    }
    let abs_val = value.unsigned_abs();
    for b in dest.iter_mut() {
        *b = b'0';
    }
    let mut val = abs_val;
    for i in (0..len).rev() {
        dest[i] = b'0' + (val % 10) as u8;
        val /= 10;
        if val == 0 {
            break;
        }
    }
}

// ---------------------------------------------------------------------------
// cobolrt_decimal_* display-format variants
//
// These are used by COMPUTE expressions. They take display-format byte
// buffers, perform arithmetic, and return a pointer to a thread-local
// result buffer. The caller provides `result_len` to size the output.
//
// NOTE: cobolrt_decimal_add/sub/mul/div are already defined in decimal.rs
// with PackedDecimal signatures. These display-format versions use the
// suffix `_display` to avoid symbol collision. Once decimal.rs stubs are
// replaced, these can be renamed.
// ---------------------------------------------------------------------------

/// Perform addition on two display-format values and return a pointer to
/// a thread-local result buffer.
///
/// # Safety
///
/// - `left` must point to at least `left_len` readable bytes, or be null.
/// - `right` must point to at least `right_len` readable bytes, or be null.
/// - The returned pointer is only valid until the next call to a
///   `cobolrt_decimal_*_display` function on the same thread.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_decimal_add_display(
    left: *const u8,
    left_len: u32,
    right: *const u8,
    right_len: u32,
    result_len: u32,
) -> *mut u8 {
    if left.is_null() || right.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: Caller guarantees `left` points to `left_len` readable bytes.
    let l_slice = std::slice::from_raw_parts(left, left_len as usize);
    // SAFETY: Caller guarantees `right` points to `right_len` readable bytes.
    let r_slice = std::slice::from_raw_parts(right, right_len as usize);

    let l = parse_display(l_slice);
    let r = parse_display(r_slice);
    let (lv, rv, scale) = align_scales(&l, &r);
    let result = lv + rv;

    write_to_thread_local(result, scale, result_len)
}

/// Subtraction variant returning pointer to thread-local buffer.
///
/// # Safety
///
/// - `left` must point to at least `left_len` readable bytes, or be null.
/// - `right` must point to at least `right_len` readable bytes, or be null.
/// - The returned pointer is only valid until the next call to a
///   `cobolrt_decimal_*_display` function on the same thread.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_decimal_sub_display(
    left: *const u8,
    left_len: u32,
    right: *const u8,
    right_len: u32,
    result_len: u32,
) -> *mut u8 {
    if left.is_null() || right.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: Caller guarantees `left` points to `left_len` readable bytes.
    let l_slice = std::slice::from_raw_parts(left, left_len as usize);
    // SAFETY: Caller guarantees `right` points to `right_len` readable bytes.
    let r_slice = std::slice::from_raw_parts(right, right_len as usize);

    let l = parse_display(l_slice);
    let r = parse_display(r_slice);
    let (lv, rv, scale) = align_scales(&l, &r);
    let result = lv - rv;

    write_to_thread_local(result, scale, result_len)
}

/// Multiplication variant returning pointer to thread-local buffer.
///
/// # Safety
///
/// - `left` must point to at least `left_len` readable bytes, or be null.
/// - `right` must point to at least `right_len` readable bytes, or be null.
/// - The returned pointer is only valid until the next call to a
///   `cobolrt_decimal_*_display` function on the same thread.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_decimal_mul_display(
    left: *const u8,
    left_len: u32,
    right: *const u8,
    right_len: u32,
    result_len: u32,
) -> *mut u8 {
    if left.is_null() || right.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: Caller guarantees `left` points to `left_len` readable bytes.
    let l_slice = std::slice::from_raw_parts(left, left_len as usize);
    // SAFETY: Caller guarantees `right` points to `right_len` readable bytes.
    let r_slice = std::slice::from_raw_parts(right, right_len as usize);

    let l = parse_display(l_slice);
    let r = parse_display(r_slice);

    let mut lv = l.digits as i128;
    if l.negative {
        lv = -lv;
    }
    let mut rv = r.digits as i128;
    if r.negative {
        rv = -rv;
    }
    let result = lv * rv;
    let result_scale = l.scale + r.scale;

    write_to_thread_local(result, result_scale, result_len)
}

/// Division variant returning pointer to thread-local buffer.
///
/// # Safety
///
/// - `left` must point to at least `left_len` readable bytes, or be null.
/// - `right` must point to at least `right_len` readable bytes, or be null.
/// - The returned pointer is only valid until the next call to a
///   `cobolrt_decimal_*_display` function on the same thread.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_decimal_div_display(
    left: *const u8,
    left_len: u32,
    right: *const u8,
    right_len: u32,
    result_len: u32,
) -> *mut u8 {
    if left.is_null() || right.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: Caller guarantees `left` points to `left_len` readable bytes.
    let l_slice = std::slice::from_raw_parts(left, left_len as usize);
    // SAFETY: Caller guarantees `right` points to `right_len` readable bytes.
    let r_slice = std::slice::from_raw_parts(right, right_len as usize);

    let l = parse_display(l_slice);
    let r = parse_display(r_slice);

    if r.digits == 0 {
        // Division by zero: return zeros.
        return write_to_thread_local(0, 0, result_len);
    }

    // Align scales so dividend and divisor are comparable.
    let (lv, rv, scale) = align_scales(&l, &r);

    // After alignment, lv / rv gives an integer result with no fractional
    // part. To preserve `scale` fractional digits in the result, multiply
    // the dividend by 10^scale before dividing.
    let mut adjusted = lv;
    for _ in 0..scale {
        adjusted *= 10;
    }
    let result = if rv != 0 { adjusted / rv } else { 0 };

    write_to_thread_local(result, scale, result_len)
}

/// Exponentiation: base ** exp, returning pointer to thread-local buffer.
///
/// # Safety
///
/// - `base` must point to at least `base_len` readable bytes, or be null.
/// - `exp` must point to at least `exp_len` readable bytes, or be null.
/// - The returned pointer is only valid until the next call to a
///   `cobolrt_decimal_*_display` function on the same thread.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_decimal_pow(
    base: *const u8,
    base_len: u32,
    exp: *const u8,
    exp_len: u32,
    result_len: u32,
) -> *mut u8 {
    if base.is_null() || exp.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: Caller guarantees `base` points to `base_len` readable bytes.
    let b_slice = std::slice::from_raw_parts(base, base_len as usize);
    // SAFETY: Caller guarantees `exp` points to `exp_len` readable bytes.
    let e_slice = std::slice::from_raw_parts(exp, exp_len as usize);

    let b = parse_display(b_slice);
    let e = parse_display(e_slice);

    // For simplicity, convert the exponent to an integer (ignoring fractional part).
    let exp_val = {
        let mut v = e.digits;
        for _ in 0..e.scale {
            v /= 10;
        }
        if e.negative {
            // Negative exponents for integer arithmetic: result is 0 for
            // bases > 1, 1 for base == 1, undefined otherwise. We return 0.
            return write_to_thread_local(0, 0, result_len);
        }
        v as u32
    };

    // Compute base_value as a signed scaled integer.
    let mut base_val = b.digits as i128;
    if b.negative {
        base_val = -base_val;
    }

    // Result scale: base_scale * exp_val
    let result_scale = b.scale * exp_val;

    // Compute power by repeated multiplication.
    let mut result: i128 = 1;
    for _ in 0..exp_val {
        result *= base_val;
    }

    write_to_thread_local(result, result_scale, result_len)
}

// ---------------------------------------------------------------------------
// Unit tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -- Helper to create a display string and call add_numeric --

    fn add(a: &[u8], b: &[u8], dest_len: usize) -> Vec<u8> {
        let mut dest = vec![b'0'; dest_len];
        // SAFETY: Test slices are valid for the given lengths.
        unsafe {
            cobolrt_add_numeric(
                a.as_ptr(),
                a.len() as u32,
                b.as_ptr(),
                b.len() as u32,
                dest.as_mut_ptr(),
                dest_len as u32,
            );
        }
        dest
    }

    fn sub(a: &[u8], b: &[u8], dest_len: usize) -> Vec<u8> {
        let mut dest = vec![b'0'; dest_len];
        // SAFETY: Test slices are valid for the given lengths.
        unsafe {
            cobolrt_subtract_numeric(
                a.as_ptr(),
                a.len() as u32,
                b.as_ptr(),
                b.len() as u32,
                dest.as_mut_ptr(),
                dest_len as u32,
            );
        }
        dest
    }

    fn mul(a: &[u8], b: &[u8], dest_len: usize) -> Vec<u8> {
        let mut dest = vec![b'0'; dest_len];
        // SAFETY: Test slices are valid for the given lengths.
        unsafe {
            cobolrt_multiply_numeric(
                a.as_ptr(),
                a.len() as u32,
                b.as_ptr(),
                b.len() as u32,
                dest.as_mut_ptr(),
                dest_len as u32,
            );
        }
        dest
    }

    // -- Addition tests --

    #[test]
    fn add_simple_integers() {
        assert_eq!(add(b"00003", b"00005", 5), b"00008");
    }

    #[test]
    fn add_with_carry() {
        assert_eq!(add(b"00099", b"00001", 5), b"00100");
    }

    #[test]
    fn add_different_lengths() {
        // 123 + 7 = 130
        assert_eq!(add(b"123", b"7", 5), b"00130");
    }

    #[test]
    fn add_larger_numbers() {
        assert_eq!(add(b"12345", b"54321", 5), b"66666");
    }

    #[test]
    fn add_null_safety() {
        let mut dest = vec![b'0'; 5];
        // SAFETY: Null pointer is handled gracefully by the function.
        unsafe {
            cobolrt_add_numeric(std::ptr::null(), 0, b"1".as_ptr(), 1, dest.as_mut_ptr(), 5);
        }
        // dest should be unchanged
        assert_eq!(dest, b"00000");
    }

    // -- Subtraction tests --

    #[test]
    fn sub_simple() {
        assert_eq!(sub(b"00009", b"00003", 5), b"00006");
    }

    #[test]
    fn sub_with_borrow() {
        assert_eq!(sub(b"00100", b"00001", 5), b"00099");
    }

    #[test]
    fn sub_zero_result() {
        assert_eq!(sub(b"00050", b"00050", 5), b"00000");
    }

    // -- Multiplication tests --

    #[test]
    fn mul_simple() {
        assert_eq!(mul(b"003", b"004", 5), b"00012");
    }

    #[test]
    fn mul_by_zero() {
        assert_eq!(mul(b"123", b"000", 5), b"00000");
    }

    #[test]
    fn mul_larger() {
        assert_eq!(mul(b"012", b"012", 5), b"00144");
    }

    // -- display_to_int tests --

    #[test]
    fn display_to_int_simple() {
        // SAFETY: Byte literal is valid for 5 bytes.
        assert_eq!(unsafe { cobolrt_display_to_int(b"00042".as_ptr(), 5) }, 42);
    }

    #[test]
    fn display_to_int_with_sign() {
        // SAFETY: Byte literal is valid for 5 bytes.
        assert_eq!(
            unsafe { cobolrt_display_to_int(b"-0123".as_ptr(), 5) },
            -123
        );
    }

    #[test]
    fn display_to_int_null() {
        // SAFETY: Null pointer is handled gracefully by the function.
        assert_eq!(unsafe { cobolrt_display_to_int(std::ptr::null(), 0) }, 0);
    }

    #[test]
    fn display_to_int_with_decimal() {
        // Decimal point is skipped; digits are concatenated.
        // SAFETY: Byte literal is valid for 5 bytes.
        assert_eq!(
            unsafe { cobolrt_display_to_int(b"12.50".as_ptr(), 5) },
            1250
        );
    }

    // -- string_append tests --

    #[test]
    fn string_append_basic() {
        let src = b"WORLD";
        let mut dest = *b"HELLO     ";
        let mut ptr = *b"06"; // 1-based position 6

        // SAFETY: All pointers are valid for their respective lengths.
        unsafe {
            cobolrt_string_append(src.as_ptr(), 5, dest.as_mut_ptr(), 10, ptr.as_mut_ptr(), 2);
        }

        assert_eq!(&dest, b"HELLOWORLD");
        assert_eq!(&ptr, b"11"); // pointer advanced to 11
    }

    #[test]
    fn string_append_partial() {
        let src = b"ABCDE";
        let mut dest = *b"XX   ";
        let mut ptr = *b"03"; // position 3 (1-based)

        // SAFETY: All pointers are valid for their respective lengths.
        unsafe {
            cobolrt_string_append(src.as_ptr(), 5, dest.as_mut_ptr(), 5, ptr.as_mut_ptr(), 2);
        }

        // Only 3 chars fit (positions 3, 4, 5)
        assert_eq!(&dest, b"XXABC");
        // Pointer advanced by 3 (how many actually copied)
        assert_eq!(&ptr, b"06");
    }

    // -- divide_scaled tests --

    #[test]
    fn divide_scaled_simple() {
        let src1 = b"010";
        let src2 = b"002";
        let mut dest = [b'0'; 5];

        // SAFETY: All pointers are valid for their respective lengths.
        unsafe {
            cobolrt_divide_scaled(
                src1.as_ptr(),
                3,
                0, // src1, len=3, scale=0
                src2.as_ptr(),
                3,
                0, // src2, len=3, scale=0
                dest.as_mut_ptr(),
                5,
                0,  // dest, len=5, scale=0
                -1, // no dot
                0,  // not rounded
            );
        }

        assert_eq!(&dest, b"00005");
    }

    #[test]
    fn divide_scaled_by_zero() {
        let src1 = b"010";
        let src2 = b"000";
        let mut dest = [b'0'; 5];

        // SAFETY: All pointers are valid for their respective lengths.
        unsafe {
            cobolrt_divide_scaled(
                src1.as_ptr(),
                3,
                0,
                src2.as_ptr(),
                3,
                0,
                dest.as_mut_ptr(),
                5,
                0,
                -1,
                0,
            );
        }

        assert_eq!(&dest, b"00000");
    }

    #[test]
    fn divide_scaled_with_decimal() {
        // 100 / 3 with 2 decimal places
        let src1 = b"100";
        let src2 = b"003";
        let mut dest = *b"000.00";

        // SAFETY: All pointers are valid for their respective lengths.
        unsafe {
            cobolrt_divide_scaled(
                src1.as_ptr(),
                3,
                0,
                src2.as_ptr(),
                3,
                0,
                dest.as_mut_ptr(),
                6,
                2,
                3, // dot at position 3
                0, // not rounded
            );
        }

        assert_eq!(&dest, b"033.33");
    }

    #[test]
    fn divide_scaled_rounded() {
        // 100 / 3 with 2 decimal places, rounded
        let src1 = b"100";
        let src2 = b"003";
        let mut dest = *b"000.00";

        // SAFETY: All pointers are valid for their respective lengths.
        unsafe {
            cobolrt_divide_scaled(
                src1.as_ptr(),
                3,
                0,
                src2.as_ptr(),
                3,
                0,
                dest.as_mut_ptr(),
                6,
                2,
                3, // dot at position 3
                1, // rounded
            );
        }

        assert_eq!(&dest, b"033.33");
    }

    // -- negate_numeric tests --

    #[test]
    fn negate_positive() {
        let src = b"00042";
        // SAFETY: `src` is valid for 5 bytes.
        let ptr = unsafe { cobolrt_negate_numeric(src.as_ptr(), 5) };
        assert!(!ptr.is_null());
        // Result has one extra byte for sign.
        let result = unsafe { std::slice::from_raw_parts(ptr, 6) };
        // Should start with '-'
        assert_eq!(result[0], b'-');
        // And contain the digits.
        assert_eq!(&result[1..], b"00042");
    }

    #[test]
    fn negate_null() {
        // SAFETY: Null pointer is handled gracefully by the function.
        let ptr = unsafe { cobolrt_negate_numeric(std::ptr::null(), 0) };
        assert!(ptr.is_null());
    }

    // -- decimal_add_display tests --

    #[test]
    fn decimal_add_display_simple() {
        let a = b"00005";
        let b = b"00003";
        // SAFETY: Both pointers are valid for 5 bytes.
        let ptr = unsafe { cobolrt_decimal_add_display(a.as_ptr(), 5, b.as_ptr(), 5, 5) };
        assert!(!ptr.is_null());
        let result = unsafe { std::slice::from_raw_parts(ptr, 5) };
        assert_eq!(result, b"00008");
    }

    #[test]
    fn decimal_sub_display_simple() {
        let a = b"00010";
        let b = b"00003";
        // SAFETY: Both pointers are valid for 5 bytes.
        let ptr = unsafe { cobolrt_decimal_sub_display(a.as_ptr(), 5, b.as_ptr(), 5, 5) };
        assert!(!ptr.is_null());
        let result = unsafe { std::slice::from_raw_parts(ptr, 5) };
        assert_eq!(result, b"00007");
    }

    #[test]
    fn decimal_mul_display_simple() {
        let a = b"004";
        let b = b"005";
        // SAFETY: Both pointers are valid for 3 bytes.
        let ptr = unsafe { cobolrt_decimal_mul_display(a.as_ptr(), 3, b.as_ptr(), 3, 5) };
        assert!(!ptr.is_null());
        let result = unsafe { std::slice::from_raw_parts(ptr, 5) };
        assert_eq!(result, b"00020");
    }

    #[test]
    fn decimal_div_display_simple() {
        let a = b"020";
        let b = b"004";
        // SAFETY: Both pointers are valid for 3 bytes.
        let ptr = unsafe { cobolrt_decimal_div_display(a.as_ptr(), 3, b.as_ptr(), 3, 5) };
        assert!(!ptr.is_null());
        let result = unsafe { std::slice::from_raw_parts(ptr, 5) };
        assert_eq!(result, b"00005");
    }

    #[test]
    fn decimal_div_display_by_zero() {
        let a = b"020";
        let b = b"000";
        // SAFETY: Both pointers are valid for 3 bytes.
        let ptr = unsafe { cobolrt_decimal_div_display(a.as_ptr(), 3, b.as_ptr(), 3, 5) };
        assert!(!ptr.is_null());
        let result = unsafe { std::slice::from_raw_parts(ptr, 5) };
        assert_eq!(result, b"00000");
    }

    #[test]
    fn decimal_pow_simple() {
        let base = b"003";
        let exp = b"004";
        // SAFETY: Both pointers are valid for 3 bytes.
        let ptr = unsafe { cobolrt_decimal_pow(base.as_ptr(), 3, exp.as_ptr(), 3, 5) };
        assert!(!ptr.is_null());
        let result = unsafe { std::slice::from_raw_parts(ptr, 5) };
        // 3^4 = 81
        assert_eq!(result, b"00081");
    }

    #[test]
    fn decimal_pow_zero_exponent() {
        let base = b"005";
        let exp = b"000";
        // SAFETY: Both pointers are valid for 3 bytes.
        let ptr = unsafe { cobolrt_decimal_pow(base.as_ptr(), 3, exp.as_ptr(), 3, 5) };
        assert!(!ptr.is_null());
        let result = unsafe { std::slice::from_raw_parts(ptr, 5) };
        // 5^0 = 1
        assert_eq!(result, b"00001");
    }

    #[test]
    fn decimal_pow_one_exponent() {
        let base = b"042";
        let exp = b"001";
        // SAFETY: Both pointers are valid for 3 bytes.
        let ptr = unsafe { cobolrt_decimal_pow(base.as_ptr(), 3, exp.as_ptr(), 3, 5) };
        assert!(!ptr.is_null());
        let result = unsafe { std::slice::from_raw_parts(ptr, 5) };
        // 42^1 = 42
        assert_eq!(result, b"00042");
    }

    // -- parse_display tests --

    #[test]
    fn parse_display_integer() {
        let p = parse_display(b"00123");
        assert_eq!(p.digits, 123);
        assert_eq!(p.scale, 0);
        assert!(!p.negative);
    }

    #[test]
    fn parse_display_decimal() {
        let p = parse_display(b"12.50");
        assert_eq!(p.digits, 1250);
        assert_eq!(p.scale, 2);
        assert!(!p.negative);
    }

    #[test]
    fn parse_display_negative() {
        let p = parse_display(b"-0042");
        assert_eq!(p.digits, 42);
        assert_eq!(p.scale, 0);
        assert!(p.negative);
    }

    // -- align_scales tests --

    #[test]
    fn align_scales_same() {
        let l = ParsedNumeric {
            digits: 100,
            scale: 2,
            negative: false,
        };
        let r = ParsedNumeric {
            digits: 200,
            scale: 2,
            negative: false,
        };
        let (lv, rv, s) = align_scales(&l, &r);
        assert_eq!(lv, 100);
        assert_eq!(rv, 200);
        assert_eq!(s, 2);
    }

    #[test]
    fn align_scales_different() {
        // 1.5 (digits=15, scale=1) and 2.50 (digits=250, scale=2)
        let l = ParsedNumeric {
            digits: 15,
            scale: 1,
            negative: false,
        };
        let r = ParsedNumeric {
            digits: 250,
            scale: 2,
            negative: false,
        };
        let (lv, rv, s) = align_scales(&l, &r);
        assert_eq!(lv, 150); // 15 * 10
        assert_eq!(rv, 250);
        assert_eq!(s, 2);
    }

    // -- write_display tests --

    #[test]
    fn write_display_simple_integer() {
        let mut buf = [b'0'; 5];
        write_display(42, 0, &mut buf);
        assert_eq!(&buf, b"00042");
    }

    #[test]
    fn write_display_with_dot() {
        let mut buf = *b"000.00";
        write_display(1250, 2, &mut buf);
        assert_eq!(&buf, b"012.50");
    }

    #[test]
    fn write_display_zero() {
        let mut buf = [b'0'; 5];
        write_display(0, 0, &mut buf);
        assert_eq!(&buf, b"00000");
    }

    // -- int_to_display tests --

    #[test]
    fn int_to_display_basic() {
        let mut buf = [b'0'; 5];
        int_to_display(42, &mut buf);
        assert_eq!(&buf, b"00042");
    }

    #[test]
    fn int_to_display_large() {
        let mut buf = [b'0'; 5];
        int_to_display(12345, &mut buf);
        assert_eq!(&buf, b"12345");
    }
}
