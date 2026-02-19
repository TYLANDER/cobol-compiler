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
///
/// Format: `YYYYMMDDHHMMSSFFshhmm`
///   - YYYY = 4-digit year
///   - MM   = month (01-12)
///   - DD   = day (01-31)
///   - HH   = hours (00-23)
///   - MM   = minutes (00-59)
///   - SS   = seconds (00-59)
///   - FF   = hundredths of seconds (00-99)
///   - s    = sign of UTC offset ('+' or '-')
///   - hhmm = UTC offset hours and minutes
///
/// Note: UTC offset is reported as +0000 (UTC) since determining local
/// timezone offset without libc/chrono is non-trivial. The date/time
/// components are UTC.
#[no_mangle]
pub extern "C" fn cobolrt_intrinsic_current_date(output: *mut u8) {
    if output.is_null() {
        return;
    }
    let (secs, millis) = crate::display::unix_now();
    let days = secs.div_euclid(86400);
    let (year, month, day) = crate::display::civil_from_days(days);
    let day_secs = secs.rem_euclid(86400) as u32;
    let hh = day_secs / 3600;
    let mm = (day_secs % 3600) / 60;
    let ss = day_secs % 60;
    let cs = millis / 10; // hundredths

    let mut buf = [b'0'; 21];
    // YYYY
    let y = year.unsigned_abs() as u32;
    buf[0] = b'0' + ((y / 1000) % 10) as u8;
    buf[1] = b'0' + ((y / 100) % 10) as u8;
    buf[2] = b'0' + ((y / 10) % 10) as u8;
    buf[3] = b'0' + (y % 10) as u8;
    // MM
    buf[4] = b'0' + (month / 10) as u8;
    buf[5] = b'0' + (month % 10) as u8;
    // DD
    buf[6] = b'0' + (day / 10) as u8;
    buf[7] = b'0' + (day % 10) as u8;
    // HH
    buf[8] = b'0' + (hh / 10) as u8;
    buf[9] = b'0' + (hh % 10) as u8;
    // MM
    buf[10] = b'0' + (mm / 10) as u8;
    buf[11] = b'0' + (mm % 10) as u8;
    // SS
    buf[12] = b'0' + (ss / 10) as u8;
    buf[13] = b'0' + (ss % 10) as u8;
    // FF (hundredths)
    buf[14] = b'0' + (cs / 10) as u8;
    buf[15] = b'0' + (cs % 10) as u8;
    // UTC offset: +0000
    buf[16] = b'+';
    buf[17] = b'0';
    buf[18] = b'0';
    buf[19] = b'0';
    buf[20] = b'0';

    unsafe {
        std::ptr::copy_nonoverlapping(buf.as_ptr(), output, 21);
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
