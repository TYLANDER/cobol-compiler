/// FUNCTION LENGTH
#[no_mangle]
pub extern "C" fn cobolrt_intrinsic_length(data: *const u8, len: u32) -> i64 {
    let _ = data;
    len as i64
}

/// FUNCTION REVERSE
#[no_mangle]
pub extern "C" fn cobolrt_intrinsic_reverse(input: *const u8, len: u32, output: *mut u8) {
    if input.is_null() || output.is_null() {
        return;
    }
    let slice = unsafe { std::slice::from_raw_parts(input, len as usize) };
    for (i, &byte) in slice.iter().rev().enumerate() {
        unsafe {
            *output.add(i) = byte;
        }
    }
}

/// FUNCTION UPPER-CASE
#[no_mangle]
pub extern "C" fn cobolrt_intrinsic_upper_case(data: *mut u8, len: u32) {
    if data.is_null() {
        return;
    }
    let slice = unsafe { std::slice::from_raw_parts_mut(data, len as usize) };
    for byte in slice.iter_mut() {
        *byte = byte.to_ascii_uppercase();
    }
}

/// FUNCTION LOWER-CASE
#[no_mangle]
pub extern "C" fn cobolrt_intrinsic_lower_case(data: *mut u8, len: u32) {
    if data.is_null() {
        return;
    }
    let slice = unsafe { std::slice::from_raw_parts_mut(data, len as usize) };
    for byte in slice.iter_mut() {
        *byte = byte.to_ascii_lowercase();
    }
}

/// FUNCTION CURRENT-DATE - returns 21-char date string (YYYYMMDDHHMMSSFF+HHMM)
#[no_mangle]
pub extern "C" fn cobolrt_intrinsic_current_date(output: *mut u8) {
    if output.is_null() {
        return;
    }
    // TODO: implement with actual date/time
    let placeholder = b"00000000000000000+0000";
    unsafe {
        std::ptr::copy_nonoverlapping(placeholder.as_ptr(), output, 21);
    }
}

/// FUNCTION WHEN-COMPILED - returns compilation timestamp
#[no_mangle]
pub extern "C" fn cobolrt_intrinsic_when_compiled(output: *mut u8) {
    if output.is_null() {
        return;
    }
    let placeholder = b"00000000000000000+0000";
    unsafe {
        std::ptr::copy_nonoverlapping(placeholder.as_ptr(), output, 21);
    }
}

/// FUNCTION NUMVAL - convert alphanumeric to numeric
#[no_mangle]
pub extern "C" fn cobolrt_intrinsic_numval(data: *const u8, len: u32) -> f64 {
    let _ = (data, len);
    // TODO: implement
    0.0
}

/// FUNCTION NUMVAL-C - convert alphanumeric with currency to numeric
#[no_mangle]
pub extern "C" fn cobolrt_intrinsic_numval_c(data: *const u8, len: u32, currency: u8) -> f64 {
    let _ = (data, len, currency);
    0.0
}

/// FUNCTION MOD
#[no_mangle]
pub extern "C" fn cobolrt_intrinsic_mod(a: i64, b: i64) -> i64 {
    if b == 0 {
        return 0;
    }
    a % b
}

/// FUNCTION INTEGER
#[no_mangle]
pub extern "C" fn cobolrt_intrinsic_integer(val: f64) -> i64 {
    val.floor() as i64
}

/// FUNCTION INTEGER-PART
#[no_mangle]
pub extern "C" fn cobolrt_intrinsic_integer_part(val: f64) -> i64 {
    val.trunc() as i64
}

/// FUNCTION ORD
#[no_mangle]
pub extern "C" fn cobolrt_intrinsic_ord(c: u8) -> i64 {
    c as i64 + 1
}

/// FUNCTION CHAR
#[no_mangle]
pub extern "C" fn cobolrt_intrinsic_char(n: i64) -> u8 {
    if n < 1 || n > 256 {
        0
    } else {
        (n - 1) as u8
    }
}

// TODO: Implement remaining ~75 intrinsic functions
