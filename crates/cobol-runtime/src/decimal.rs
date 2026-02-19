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

/// Add two packed decimals, storing result in `result`
#[no_mangle]
pub extern "C" fn cobolrt_decimal_add(
    left: *const PackedDecimal,
    right: *const PackedDecimal,
    result: *mut PackedDecimal,
    rounded: bool,
) {
    // TODO: implement packed decimal addition
    let _ = (left, right, result, rounded);
}

/// Subtract right from left
#[no_mangle]
pub extern "C" fn cobolrt_decimal_sub(
    left: *const PackedDecimal,
    right: *const PackedDecimal,
    result: *mut PackedDecimal,
    rounded: bool,
) {
    let _ = (left, right, result, rounded);
}

/// Multiply two packed decimals
#[no_mangle]
pub extern "C" fn cobolrt_decimal_mul(
    left: *const PackedDecimal,
    right: *const PackedDecimal,
    result: *mut PackedDecimal,
    rounded: bool,
) {
    let _ = (left, right, result, rounded);
}

/// Divide left by right
#[no_mangle]
pub extern "C" fn cobolrt_decimal_div(
    left: *const PackedDecimal,
    right: *const PackedDecimal,
    result: *mut PackedDecimal,
    remainder: *mut PackedDecimal, // may be null
    rounded: bool,
) -> i32 {
    // Returns 0 on success, -1 on division by zero
    let _ = (left, right, result, remainder, rounded);
    0
}

/// Compare two packed decimals
/// Returns -1, 0, or 1
#[no_mangle]
pub extern "C" fn cobolrt_decimal_cmp(
    left: *const PackedDecimal,
    right: *const PackedDecimal,
) -> i32 {
    let _ = (left, right);
    0
}

/// Convert display (USAGE DISPLAY) numeric to packed decimal
#[no_mangle]
pub extern "C" fn cobolrt_display_to_packed(
    display_data: *const u8,
    display_len: u32,
    result: *mut PackedDecimal,
) {
    let _ = (display_data, display_len, result);
}

/// Convert packed decimal to display format
#[no_mangle]
pub extern "C" fn cobolrt_packed_to_display(
    packed: *const PackedDecimal,
    display_data: *mut u8,
    display_len: u32,
) {
    let _ = (packed, display_data, display_len);
}

/// Check if a decimal operation resulted in size error (overflow)
#[no_mangle]
pub extern "C" fn cobolrt_decimal_check_size_error(
    value: *const PackedDecimal,
    max_digits: u16,
) -> bool {
    let _ = (value, max_digits);
    false
}
