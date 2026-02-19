/// Compare two display-format numeric values.
/// Returns: -1 if left < right, 0 if equal, 1 if left > right.
/// Both buffers contain ASCII digit characters (optionally with '.' or leading sign).
///
/// # Safety
///
/// - `left` must point to at least `left_len` readable bytes, or be null.
/// - `right` must point to at least `right_len` readable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_compare_numeric(
    left: *const u8,
    left_len: u32,
    right: *const u8,
    right_len: u32,
) -> i32 {
    if left.is_null() || right.is_null() {
        return 0;
    }
    // SAFETY: Caller guarantees `left` points to `left_len` readable bytes.
    let l_slice = std::slice::from_raw_parts(left, left_len as usize);
    // SAFETY: Caller guarantees `right` points to `right_len` readable bytes.
    let r_slice = std::slice::from_raw_parts(right, right_len as usize);

    // Parse both as numeric values: extract integer and fractional parts
    let l_val = parse_display_numeric(l_slice);
    let r_val = parse_display_numeric(r_slice);

    // Compare as (sign, integer_part, frac_part) tuples
    if l_val < r_val {
        -1
    } else if l_val > r_val {
        1
    } else {
        0
    }
}

/// Compare two alphanumeric values character by character.
/// Shorter string is padded with spaces on the right.
/// Returns: -1 if left < right, 0 if equal, 1 if left > right.
///
/// # Safety
///
/// - `left` must point to at least `left_len` readable bytes, or be null.
/// - `right` must point to at least `right_len` readable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_compare_alphanumeric(
    left: *const u8,
    left_len: u32,
    right: *const u8,
    right_len: u32,
) -> i32 {
    if left.is_null() || right.is_null() {
        return 0;
    }
    // SAFETY: Caller guarantees `left` points to `left_len` readable bytes.
    let l_slice = std::slice::from_raw_parts(left, left_len as usize);
    // SAFETY: Caller guarantees `right` points to `right_len` readable bytes.
    let r_slice = std::slice::from_raw_parts(right, right_len as usize);

    let max_len = std::cmp::max(l_slice.len(), r_slice.len());
    for i in 0..max_len {
        let l_byte = if i < l_slice.len() { l_slice[i] } else { b' ' };
        let r_byte = if i < r_slice.len() { r_slice[i] } else { b' ' };
        if l_byte < r_byte {
            return -1;
        }
        if l_byte > r_byte {
            return 1;
        }
    }
    0
}

/// Parse a display-format numeric string into an i128 scaled value for comparison.
/// Handles: leading/trailing sign, embedded decimal point, leading zeros.
fn parse_display_numeric(data: &[u8]) -> i128 {
    let mut negative = false;
    let mut int_part: i128 = 0;
    let mut frac_part: i128 = 0;
    let mut frac_digits: u32 = 0;
    let mut in_frac = false;

    for &b in data {
        match b {
            b'-' => negative = true,
            b'+' => {}
            b'.' => in_frac = true,
            b'0'..=b'9' => {
                if in_frac {
                    frac_part = frac_part * 10 + (b - b'0') as i128;
                    frac_digits += 1;
                } else {
                    int_part = int_part * 10 + (b - b'0') as i128;
                }
            }
            _ => {} // skip spaces, other chars
        }
    }

    // Normalize: scale to a common representation
    // Use 18 fractional digits for precision
    const SCALE: u32 = 18;
    let mut value = int_part;
    for _ in 0..SCALE {
        value *= 10;
    }
    if frac_digits <= SCALE {
        let mut scaled_frac = frac_part;
        for _ in 0..(SCALE - frac_digits) {
            scaled_frac *= 10;
        }
        value += scaled_frac;
    } else {
        let mut scaled_frac = frac_part;
        for _ in 0..(frac_digits - SCALE) {
            scaled_frac /= 10;
        }
        value += scaled_frac;
    }

    if negative {
        -value
    } else {
        value
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compare_equal_single_digit() {
        let a = b"3";
        let b = b"3";
        // SAFETY: Both pointers are valid for 1 byte.
        assert_eq!(unsafe { cobolrt_compare_numeric(a.as_ptr(), 1, b.as_ptr(), 1) }, 0);
    }

    #[test]
    fn compare_less() {
        let a = b"3";
        let b = b"5";
        // SAFETY: Both pointers are valid for 1 byte.
        assert_eq!(unsafe { cobolrt_compare_numeric(a.as_ptr(), 1, b.as_ptr(), 1) }, -1);
    }

    #[test]
    fn compare_greater() {
        let a = b"9";
        let b = b"3";
        // SAFETY: Both pointers are valid for 1 byte.
        assert_eq!(unsafe { cobolrt_compare_numeric(a.as_ptr(), 1, b.as_ptr(), 1) }, 1);
    }

    #[test]
    fn compare_different_lengths() {
        let a = b"03";
        let b = b"3";
        // SAFETY: Both pointers are valid for their respective lengths.
        assert_eq!(unsafe { cobolrt_compare_numeric(a.as_ptr(), 2, b.as_ptr(), 1) }, 0);
    }

    #[test]
    fn compare_with_decimal() {
        let a = b"12.50";
        let b = b"12.50";
        // SAFETY: Both pointers are valid for 5 bytes.
        assert_eq!(unsafe { cobolrt_compare_numeric(a.as_ptr(), 5, b.as_ptr(), 5) }, 0);
    }

    #[test]
    fn compare_alpha_equal() {
        let a = b"HELLO";
        let b = b"HELLO";
        assert_eq!(
            // SAFETY: Both pointers are valid for 5 bytes.
            unsafe { cobolrt_compare_alphanumeric(a.as_ptr(), 5, b.as_ptr(), 5) },
            0
        );
    }

    #[test]
    fn compare_alpha_with_padding() {
        let a = b"HI";
        let b = b"HI   ";
        assert_eq!(
            // SAFETY: Both pointers are valid for their respective lengths.
            unsafe { cobolrt_compare_alphanumeric(a.as_ptr(), 2, b.as_ptr(), 5) },
            0
        );
    }

    #[test]
    fn compare_alpha_less() {
        let a = b"ABC";
        let b = b"ABD";
        assert_eq!(
            // SAFETY: Both pointers are valid for 3 bytes.
            unsafe { cobolrt_compare_alphanumeric(a.as_ptr(), 3, b.as_ptr(), 3) },
            -1
        );
    }
}
