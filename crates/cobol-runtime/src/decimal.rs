/// Packed BCD representation
/// digits: number of significant digits
/// scale: number of decimal places
/// data: raw BCD bytes (2 digits per byte, sign in low nibble of last byte)
#[repr(C)]
pub struct PackedDecimal {
    pub data: *mut u8,
    pub len: u32,
    pub digits: u16,
    pub scale: i16,
}

/// Helper: extract the integer value from a PackedDecimal.
/// Packed BCD stores 2 digits per byte, with the sign in the low nibble
/// of the last byte (0x0C = positive, 0x0D = negative).
unsafe fn packed_to_i128(pd: *const PackedDecimal) -> i128 {
    if pd.is_null() || (*pd).data.is_null() || (*pd).len == 0 {
        return 0;
    }
    let data = std::slice::from_raw_parts((*pd).data, (*pd).len as usize);
    let mut value: i128 = 0;
    let last_byte_idx = data.len() - 1;

    for (i, &byte) in data.iter().enumerate() {
        let high = (byte >> 4) & 0x0F;
        let low = byte & 0x0F;

        if i < last_byte_idx {
            // Both nibbles are digits
            value = value * 10 + high as i128;
            value = value * 10 + low as i128;
        } else {
            // Last byte: high nibble is the last digit, low nibble is the sign
            value = value * 10 + high as i128;
            if low == 0x0D {
                value = -value;
            }
        }
    }
    value
}

/// Helper: store an integer value into a PackedDecimal.
unsafe fn i128_to_packed(value: i128, pd: *mut PackedDecimal) {
    if pd.is_null() || (*pd).data.is_null() || (*pd).len == 0 {
        return;
    }
    let data = std::slice::from_raw_parts_mut((*pd).data, (*pd).len as usize);
    let sign: u8 = if value < 0 { 0x0D } else { 0x0C };
    let mut abs_val = value.unsigned_abs();

    // Fill from the last byte backwards
    let last_byte_idx = data.len() - 1;

    // Last byte: high nibble = last digit, low nibble = sign
    let last_digit = (abs_val % 10) as u8;
    abs_val /= 10;
    data[last_byte_idx] = (last_digit << 4) | sign;

    // Fill remaining bytes from right to left
    for i in (0..last_byte_idx).rev() {
        let low = (abs_val % 10) as u8;
        abs_val /= 10;
        let high = (abs_val % 10) as u8;
        abs_val /= 10;
        data[i] = (high << 4) | low;
    }
}

/// Scale a value: multiply or divide by powers of 10 to adjust scale.
fn scale_value(value: i128, from_scale: i16, to_scale: i16, rounded: bool) -> i128 {
    let diff = to_scale as i32 - from_scale as i32;
    if diff > 0 {
        // Need more decimal places: multiply
        let mut result = value;
        for _ in 0..diff {
            result *= 10;
        }
        result
    } else if diff < 0 {
        // Need fewer decimal places: divide (possibly with rounding)
        let mut result = value;
        let abs_diff = (-diff) as u32;
        let mut divisor: i128 = 1;
        for _ in 0..abs_diff {
            divisor *= 10;
        }
        if rounded {
            // Round half up
            let remainder = result % divisor;
            result /= divisor;
            if remainder.abs() * 2 >= divisor {
                if result >= 0 {
                    result += 1;
                } else {
                    result -= 1;
                }
            }
            result
        } else {
            result / divisor
        }
    } else {
        value
    }
}

/// Add two packed decimals, storing result in `result`.
/// Handles different scales by normalizing to the result's scale.
///
/// # Safety
///
/// - `left` must point to a valid `PackedDecimal` with valid `data` pointer, or be null.
/// - `right` must point to a valid `PackedDecimal` with valid `data` pointer, or be null.
/// - `result` must point to a valid, writable `PackedDecimal` with valid `data` pointer, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_decimal_add(
    left: *const PackedDecimal,
    right: *const PackedDecimal,
    result: *mut PackedDecimal,
    rounded: bool,
) {
    if left.is_null() || right.is_null() || result.is_null() {
        return;
    }
    // SAFETY: Caller guarantees all pointers are valid PackedDecimal structs.
    let left_val = packed_to_i128(left);
    let right_val = packed_to_i128(right);
    let result_scale = (*result).scale;

    // Normalize both values to the result scale
    let left_scaled = scale_value(left_val, (*left).scale, result_scale, false);
    let right_scaled = scale_value(right_val, (*right).scale, result_scale, false);

    let sum = left_scaled + right_scaled;
    let final_val = if rounded {
        sum // already at result scale
    } else {
        sum
    };
    i128_to_packed(final_val, result);
}

/// Subtract right from left, storing result in `result`.
/// Computes: result = left - right
///
/// # Safety
///
/// - `left` must point to a valid `PackedDecimal` with valid `data` pointer, or be null.
/// - `right` must point to a valid `PackedDecimal` with valid `data` pointer, or be null.
/// - `result` must point to a valid, writable `PackedDecimal` with valid `data` pointer, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_decimal_sub(
    left: *const PackedDecimal,
    right: *const PackedDecimal,
    result: *mut PackedDecimal,
    rounded: bool,
) {
    if left.is_null() || right.is_null() || result.is_null() {
        return;
    }
    // SAFETY: Caller guarantees all pointers are valid PackedDecimal structs.
    let left_val = packed_to_i128(left);
    let right_val = packed_to_i128(right);
    let result_scale = (*result).scale;

    let left_scaled = scale_value(left_val, (*left).scale, result_scale, false);
    let right_scaled = scale_value(right_val, (*right).scale, result_scale, false);

    let diff = left_scaled - right_scaled;
    let final_val = if rounded { diff } else { diff };
    i128_to_packed(final_val, result);
}

/// Multiply two packed decimals, storing result in `result`.
/// The product scale = left_scale + right_scale, then adjusted to result_scale.
///
/// # Safety
///
/// - `left` must point to a valid `PackedDecimal` with valid `data` pointer, or be null.
/// - `right` must point to a valid `PackedDecimal` with valid `data` pointer, or be null.
/// - `result` must point to a valid, writable `PackedDecimal` with valid `data` pointer, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_decimal_mul(
    left: *const PackedDecimal,
    right: *const PackedDecimal,
    result: *mut PackedDecimal,
    rounded: bool,
) {
    if left.is_null() || right.is_null() || result.is_null() {
        return;
    }
    // SAFETY: Caller guarantees all pointers are valid PackedDecimal structs.
    let left_val = packed_to_i128(left);
    let right_val = packed_to_i128(right);

    // Raw product has scale = left_scale + right_scale
    let product = left_val * right_val;
    let product_scale = (*left).scale + (*right).scale;
    let result_scale = (*result).scale;

    let final_val = scale_value(product, product_scale, result_scale, rounded);
    i128_to_packed(final_val, result);
}

/// Divide left by right, storing quotient in `result` and optional remainder.
/// Returns 0 on success, -1 on division by zero.
///
/// # Safety
///
/// - `left` must point to a valid `PackedDecimal` with valid `data` pointer, or be null.
/// - `right` must point to a valid `PackedDecimal` with valid `data` pointer, or be null.
/// - `result` must point to a valid, writable `PackedDecimal` with valid `data` pointer, or be null.
/// - `remainder` must point to a valid, writable `PackedDecimal` with valid `data` pointer, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_decimal_div(
    left: *const PackedDecimal,
    right: *const PackedDecimal,
    result: *mut PackedDecimal,
    remainder: *mut PackedDecimal, // may be null
    rounded: bool,
) -> i32 {
    if left.is_null() || right.is_null() || result.is_null() {
        return -1;
    }
    // SAFETY: Caller guarantees all pointers are valid PackedDecimal structs.
    let left_val = packed_to_i128(left);
    let right_val = packed_to_i128(right);

    if right_val == 0 {
        return -1; // division by zero
    }

    let result_scale = (*result).scale;
    let left_scale = (*left).scale;
    let right_scale = (*right).scale;

    // To compute left / right at the desired result scale:
    // We need: (left_val / 10^left_scale) / (right_val / 10^right_scale) * 10^result_scale
    // = left_val * 10^(result_scale - left_scale + right_scale) / right_val
    let extra_scale = result_scale as i32 - left_scale as i32 + right_scale as i32;

    let adjusted_left = if extra_scale > 0 {
        let mut v = left_val;
        for _ in 0..extra_scale {
            v *= 10;
        }
        v
    } else if extra_scale < 0 {
        let mut v = left_val;
        for _ in 0..(-extra_scale) {
            v /= 10;
        }
        v
    } else {
        left_val
    };

    let quotient = if rounded {
        // Round half up
        let q = adjusted_left / right_val;
        let r = adjusted_left % right_val;
        if r.abs() * 2 >= right_val.abs() {
            if (adjusted_left >= 0) == (right_val >= 0) {
                q + 1
            } else {
                q - 1
            }
        } else {
            q
        }
    } else {
        adjusted_left / right_val
    };

    i128_to_packed(quotient, result);

    // Compute remainder if requested
    if !remainder.is_null() {
        let remainder_scale = (*remainder).scale;
        // remainder = left - quotient * right (at remainder's scale)
        let q_actual = quotient; // at result_scale
                                 // Convert quotient back to left's scale for remainder calculation
        let q_at_left_scale = scale_value(q_actual, result_scale, left_scale, false);
        let product_at_left = q_at_left_scale * right_val;
        // product has scale = left_scale + right_scale
        let product_scale = left_scale + right_scale;
        let product_at_left_scale = scale_value(product_at_left, product_scale, left_scale, false);
        let rem = left_val - product_at_left_scale;
        let rem_final = scale_value(rem, left_scale, remainder_scale, false);
        i128_to_packed(rem_final, remainder);
    }

    0
}

/// Compare two packed decimals.
/// Returns -1 if left < right, 0 if equal, 1 if left > right.
///
/// # Safety
///
/// - `left` must point to a valid `PackedDecimal` with valid `data` pointer, or be null.
/// - `right` must point to a valid `PackedDecimal` with valid `data` pointer, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_decimal_cmp(
    left: *const PackedDecimal,
    right: *const PackedDecimal,
) -> i32 {
    if left.is_null() || right.is_null() {
        return 0;
    }
    // SAFETY: Caller guarantees both pointers are valid PackedDecimal structs.
    let left_val = packed_to_i128(left);
    let right_val = packed_to_i128(right);
    let left_scale = (*left).scale;
    let right_scale = (*right).scale;

    // Normalize to a common scale (the larger one)
    let common_scale = left_scale.max(right_scale);
    let left_norm = scale_value(left_val, left_scale, common_scale, false);
    let right_norm = scale_value(right_val, right_scale, common_scale, false);

    if left_norm < right_norm {
        -1
    } else if left_norm > right_norm {
        1
    } else {
        0
    }
}

/// Convert display (USAGE DISPLAY) numeric to packed decimal.
/// Display format: ASCII digits, e.g., "01234" for the value 1234.
///
/// # Safety
///
/// - `display_data` must point to at least `display_len` readable bytes, or be null.
/// - `result` must point to a valid, writable `PackedDecimal` with valid `data` pointer, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_display_to_packed(
    display_data: *const u8,
    display_len: u32,
    result: *mut PackedDecimal,
) {
    if display_data.is_null() || result.is_null() || (*result).data.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `display_data` points to `display_len` readable bytes.
    let data = std::slice::from_raw_parts(display_data, display_len as usize);

    // Parse the display digits into a value
    let mut value: i128 = 0;
    let mut negative = false;
    for &byte in data {
        if byte == b'-' {
            negative = true;
        } else if byte.is_ascii_digit() {
            value = value * 10 + (byte - b'0') as i128;
        }
    }
    if negative {
        value = -value;
    }

    i128_to_packed(value, result);
}

/// Convert packed decimal to display format (ASCII digits, right-justified, zero-padded).
///
/// # Safety
///
/// - `packed` must point to a valid `PackedDecimal` with valid `data` pointer, or be null.
/// - `display_data` must point to at least `display_len` writable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_packed_to_display(
    packed: *const PackedDecimal,
    display_data: *mut u8,
    display_len: u32,
) {
    if packed.is_null() || display_data.is_null() {
        return;
    }
    // SAFETY: Caller guarantees pointers are valid.
    let value = packed_to_i128(packed);
    let out = std::slice::from_raw_parts_mut(display_data, display_len as usize);

    // Fill with ASCII zeros
    for byte in out.iter_mut() {
        *byte = b'0';
    }

    // Write digits right-to-left
    let mut abs_val = value.unsigned_abs();
    for i in (0..display_len as usize).rev() {
        if abs_val == 0 {
            break;
        }
        out[i] = b'0' + (abs_val % 10) as u8;
        abs_val /= 10;
    }
}

/// Check if a decimal operation resulted in size error (overflow).
/// Returns true if the value has more significant digits than max_digits.
///
/// # Safety
///
/// - `value` must point to a valid `PackedDecimal` with valid `data` pointer, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_decimal_check_size_error(
    value: *const PackedDecimal,
    max_digits: u16,
) -> bool {
    if value.is_null() {
        return false;
    }
    // SAFETY: Caller guarantees `value` points to a valid PackedDecimal.
    let val = packed_to_i128(value);
    let abs_val = val.unsigned_abs();

    // Count digits
    if abs_val == 0 {
        return false;
    }
    let mut digits: u16 = 0;
    let mut v = abs_val;
    while v > 0 {
        digits += 1;
        v /= 10;
    }

    // Subtract scale digits (they are decimal places, not integer overflow)
    let scale = (*value).scale;
    let integer_digits = if scale > 0 && digits > scale as u16 {
        digits - scale as u16
    } else if scale > 0 {
        0
    } else {
        digits
    };

    integer_digits > max_digits
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_packed(digits: &[u8], sign_positive: bool) -> (Vec<u8>, PackedDecimal) {
        // Pack digits into BCD format
        // Number of bytes needed: (digits.len() + 1) / 2 rounded up, +1 for sign nibble
        let total_nibbles = digits.len() + 1; // digits + sign
        let num_bytes = total_nibbles.div_ceil(2);
        let mut data = vec![0u8; num_bytes];

        // Pad digits on the left to fill the available nibbles
        let available_digit_nibbles = num_bytes * 2 - 1; // last nibble is sign
        let padding = available_digit_nibbles - digits.len();

        let mut all_nibbles: Vec<u8> = vec![0; padding];
        all_nibbles.extend_from_slice(digits);
        all_nibbles.push(if sign_positive { 0x0C } else { 0x0D });

        for i in 0..num_bytes {
            data[i] = (all_nibbles[i * 2] << 4) | all_nibbles[i * 2 + 1];
        }

        let pd = PackedDecimal {
            data: data.as_mut_ptr(),
            len: num_bytes as u32,
            digits: digits.len() as u16,
            scale: 0,
        };
        (data, pd)
    }

    #[test]
    fn test_packed_roundtrip() {
        let (_data, pd) = make_packed(&[1, 2, 3], true);
        let pd_ptr = &pd as *const PackedDecimal;
        unsafe {
            assert_eq!(packed_to_i128(pd_ptr), 123);
        }

        // Write back
        let pd_mut = &pd as *const PackedDecimal as *mut PackedDecimal;
        unsafe {
            i128_to_packed(456, pd_mut);
            assert_eq!(packed_to_i128(pd_ptr), 456);
        }
    }

    #[test]
    fn test_decimal_add() {
        let (_d1, pd1) = make_packed(&[1, 0, 0], true);
        let (_d2, pd2) = make_packed(&[2, 0, 0], true);
        let (_d3, mut pd3) = make_packed(&[0, 0, 0], true);

        // SAFETY: All PackedDecimal structs have valid data pointers.
        unsafe { cobolrt_decimal_add(&pd1, &pd2, &mut pd3, false) };
        unsafe {
            assert_eq!(packed_to_i128(&pd3), 300);
        }
    }

    #[test]
    fn test_decimal_sub() {
        let (_d1, pd1) = make_packed(&[5, 0, 0], true);
        let (_d2, pd2) = make_packed(&[2, 0, 0], true);
        let (_d3, mut pd3) = make_packed(&[0, 0, 0], true);

        // SAFETY: All PackedDecimal structs have valid data pointers.
        unsafe { cobolrt_decimal_sub(&pd1, &pd2, &mut pd3, false) };
        unsafe {
            assert_eq!(packed_to_i128(&pd3), 300);
        }
    }

    #[test]
    fn test_decimal_mul() {
        let (_d1, pd1) = make_packed(&[1, 2], true);
        let (_d2, pd2) = make_packed(&[1, 0], true);
        let (_d3, mut pd3) = make_packed(&[0, 0, 0], true);

        // SAFETY: All PackedDecimal structs have valid data pointers.
        unsafe { cobolrt_decimal_mul(&pd1, &pd2, &mut pd3, false) };
        unsafe {
            assert_eq!(packed_to_i128(&pd3), 120);
        }
    }

    #[test]
    fn test_decimal_div() {
        let (_d1, pd1) = make_packed(&[1, 0, 0], true);
        let (_d2, pd2) = make_packed(&[0, 0, 5], true);
        let (_d3, mut pd3) = make_packed(&[0, 0, 0], true);

        // SAFETY: All PackedDecimal structs have valid data pointers.
        let ret = unsafe { cobolrt_decimal_div(&pd1, &pd2, &mut pd3, std::ptr::null_mut(), false) };
        assert_eq!(ret, 0);
        unsafe {
            assert_eq!(packed_to_i128(&pd3), 20);
        }
    }

    #[test]
    fn test_decimal_div_by_zero() {
        let (_d1, pd1) = make_packed(&[1, 0, 0], true);
        let (_d2, pd2) = make_packed(&[0, 0, 0], true);
        let (_d3, mut pd3) = make_packed(&[0, 0, 0], true);

        // SAFETY: All PackedDecimal structs have valid data pointers.
        let ret = unsafe { cobolrt_decimal_div(&pd1, &pd2, &mut pd3, std::ptr::null_mut(), false) };
        assert_eq!(ret, -1); // division by zero
    }

    #[test]
    fn test_decimal_cmp() {
        let (_d1, pd1) = make_packed(&[1, 0, 0], true);
        let (_d2, pd2) = make_packed(&[2, 0, 0], true);

        // SAFETY: All PackedDecimal structs have valid data pointers.
        unsafe {
            assert_eq!(cobolrt_decimal_cmp(&pd1, &pd2), -1);
            assert_eq!(cobolrt_decimal_cmp(&pd2, &pd1), 1);
            assert_eq!(cobolrt_decimal_cmp(&pd1, &pd1), 0);
        }
    }

    #[test]
    fn test_size_error() {
        let (_data, pd) = make_packed(&[9, 9, 9], true);
        // SAFETY: PackedDecimal struct has a valid data pointer.
        unsafe {
            assert!(!cobolrt_decimal_check_size_error(&pd, 3));
            assert!(cobolrt_decimal_check_size_error(&pd, 2));
        }
    }
}
