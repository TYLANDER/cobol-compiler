use std::io::Write;

/// Display a null-terminated string to stdout (DISPLAY ... UPON CONSOLE)
///
/// # Safety
///
/// - `data` must point to at least `len` readable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_display(data: *const u8, len: u32) {
    if data.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `data` points to `len` readable bytes.
    let slice = std::slice::from_raw_parts(data, len as usize);
    let _ = std::io::stdout().write_all(slice);
}

/// Display a string followed by a newline
///
/// # Safety
///
/// - `data` must point to at least `len` readable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_display_line(data: *const u8, len: u32) {
    if data.is_null() {
        println!();
        return;
    }
    // SAFETY: Caller guarantees `data` points to `len` readable bytes.
    let slice = std::slice::from_raw_parts(data, len as usize);
    let _ = std::io::stdout().write_all(slice);
    let _ = std::io::stdout().write_all(b"\n");
    let _ = std::io::stdout().flush();
}

/// Display a numeric value formatted according to PIC
///
/// # Safety
///
/// - `data` must point to at least `len` readable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_display_numeric(
    data: *const u8,
    len: u32,
    pic_digits: u16,
    pic_scale: i16,
) {
    let _ = (data, len, pic_digits, pic_scale);
    // TODO: implement numeric display formatting
}

/// ACCEPT FROM CONSOLE - read a line from stdin
///
/// # Safety
///
/// - `buffer` must point to at least `buffer_len` writable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_accept(buffer: *mut u8, buffer_len: u32) -> u32 {
    if buffer.is_null() {
        return 0;
    }
    let mut input = String::new();
    match std::io::stdin().read_line(&mut input) {
        Ok(_) => {
            let input = input.trim_end_matches('\n').trim_end_matches('\r');
            let bytes = input.as_bytes();
            let copy_len = std::cmp::min(bytes.len(), buffer_len as usize);
            // SAFETY: Caller guarantees `buffer` points to `buffer_len` writable bytes.
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), buffer, copy_len);
            // Pad with spaces (COBOL standard)
            if copy_len < buffer_len as usize {
                std::ptr::write_bytes(buffer.add(copy_len), b' ', buffer_len as usize - copy_len);
            }
            copy_len as u32
        }
        Err(_) => 0,
    }
}

/// STOP RUN - terminate program
#[no_mangle]
pub extern "C" fn cobolrt_stop_run(status: i32) -> ! {
    std::process::exit(status)
}

// ---------------------------------------------------------------------------
// Date/time helpers (chrono-free, using std::time only)
// ---------------------------------------------------------------------------

/// Convert a Unix epoch day count (days since 1970-01-01) to (year, month, day).
///
/// Uses Howard Hinnant's `civil_from_days` algorithm.
/// Reference: <https://howardhinnant.github.io/date_algorithms.html>
pub(crate) fn civil_from_days(days: i64) -> (i64, u32, u32) {
    let z = days + 719468; // shift epoch from 1970-01-01 to 0000-03-01
    let era = if z >= 0 { z } else { z - 146096 } / 146097;
    let doe = (z - era * 146097) as u32; // day of era [0, 146096]
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365; // year of era [0, 399]
    let y = yoe as i64 + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100); // day of year [0, 365]
    let mp = (5 * doy + 2) / 153; // [0, 11]
    let d = doy - (153 * mp + 2) / 5 + 1; // day [1, 31]
    let m = if mp < 10 { mp + 3 } else { mp - 9 }; // month [1, 12]
    let y = if m <= 2 { y + 1 } else { y };
    (y, m, d)
}

/// Returns true if `year` is a leap year.
fn is_leap_year(year: i64) -> bool {
    (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
}

/// Day-of-year (1-based) for the given date.
pub(crate) fn day_of_year(year: i64, month: u32, day: u32) -> u32 {
    let cumulative = [0u32, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];
    let mut doy = cumulative[(month - 1) as usize] + day;
    if month > 2 && is_leap_year(year) {
        doy += 1;
    }
    doy
}

/// Day of week from Unix epoch day count (ISO 8601: 1=Monday .. 7=Sunday).
fn day_of_week_from_days(days: i64) -> u32 {
    // 1970-01-01 was a Thursday (ISO day 4).
    let dow = ((days % 7) + 4) % 7; // 0=Sun, 1=Mon, ..., 6=Sat (intermediate)
                                    // Convert to ISO: 1=Mon .. 7=Sun
    if dow == 0 {
        7
    } else {
        dow as u32
    }
}

/// Get current Unix timestamp components: (total_seconds, subsec_millis).
pub(crate) fn unix_now() -> (i64, u32) {
    use std::time::{SystemTime, UNIX_EPOCH};
    let dur = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    (dur.as_secs() as i64, (dur.subsec_millis()))
}

/// Write `formatted` bytes into the dest buffer.
/// If dest_len < formatted.len(), truncate.
/// If dest_len > formatted.len(), right-pad with spaces.
///
/// # Safety
///
/// - `dest` must point to at least `dest_len` writable bytes.
unsafe fn write_to_dest(dest: *mut u8, dest_len: u32, formatted: &[u8]) {
    let dest_len = dest_len as usize;
    let copy_len = std::cmp::min(formatted.len(), dest_len);
    // SAFETY: Caller guarantees `dest` points to `dest_len` writable bytes.
    std::ptr::copy_nonoverlapping(formatted.as_ptr(), dest, copy_len);
    if copy_len < dest_len {
        std::ptr::write_bytes(dest.add(copy_len), b' ', dest_len - copy_len);
    }
}

// ---------------------------------------------------------------------------
// ACCEPT DATE/TIME runtime functions
// ---------------------------------------------------------------------------

/// ACCEPT identifier FROM DATE — writes YYMMDD (6 ASCII digits).
///
/// # Safety
///
/// - `dest` must point to at least `dest_len` writable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_accept_date(dest: *mut u8, dest_len: u32) {
    if dest.is_null() {
        return;
    }
    let (secs, _) = unix_now();
    let days = secs.div_euclid(86400);
    let (year, month, day) = civil_from_days(days);
    let yy = (year % 100) as u32;
    let buf: [u8; 6] = [
        b'0' + (yy / 10) as u8,
        b'0' + (yy % 10) as u8,
        b'0' + (month / 10) as u8,
        b'0' + (month % 10) as u8,
        b'0' + (day / 10) as u8,
        b'0' + (day % 10) as u8,
    ];
    write_to_dest(dest, dest_len, &buf);
}

/// ACCEPT identifier FROM DAY — writes YYDDD (Julian day, 5 ASCII digits).
///
/// # Safety
///
/// - `dest` must point to at least `dest_len` writable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_accept_day(dest: *mut u8, dest_len: u32) {
    if dest.is_null() {
        return;
    }
    let (secs, _) = unix_now();
    let days = secs.div_euclid(86400);
    let (year, month, day) = civil_from_days(days);
    let yy = (year % 100) as u32;
    let ddd = day_of_year(year, month, day);
    let buf: [u8; 5] = [
        b'0' + (yy / 10) as u8,
        b'0' + (yy % 10) as u8,
        b'0' + (ddd / 100) as u8,
        b'0' + ((ddd / 10) % 10) as u8,
        b'0' + (ddd % 10) as u8,
    ];
    write_to_dest(dest, dest_len, &buf);
}

/// ACCEPT identifier FROM TIME — writes HHMMSSss (8 ASCII digits, ss = hundredths).
///
/// # Safety
///
/// - `dest` must point to at least `dest_len` writable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_accept_time(dest: *mut u8, dest_len: u32) {
    if dest.is_null() {
        return;
    }
    let (secs, millis) = unix_now();
    let day_secs = secs.rem_euclid(86400) as u32;
    let hh = day_secs / 3600;
    let mm = (day_secs % 3600) / 60;
    let ss = day_secs % 60;
    let cs = millis / 10; // hundredths of seconds
    let buf: [u8; 8] = [
        b'0' + (hh / 10) as u8,
        b'0' + (hh % 10) as u8,
        b'0' + (mm / 10) as u8,
        b'0' + (mm % 10) as u8,
        b'0' + (ss / 10) as u8,
        b'0' + (ss % 10) as u8,
        b'0' + (cs / 10) as u8,
        b'0' + (cs % 10) as u8,
    ];
    write_to_dest(dest, dest_len, &buf);
}

/// ACCEPT identifier FROM DAY-OF-WEEK — writes a single ASCII digit (1=Mon .. 7=Sun).
///
/// # Safety
///
/// - `dest` must point to at least `dest_len` writable bytes, or be null.
#[no_mangle]
pub unsafe extern "C" fn cobolrt_accept_day_of_week(dest: *mut u8, dest_len: u32) {
    if dest.is_null() {
        return;
    }
    let (secs, _) = unix_now();
    let days = secs.div_euclid(86400);
    let dow = day_of_week_from_days(days);
    let buf: [u8; 1] = [b'0' + dow as u8];
    write_to_dest(dest, dest_len, &buf);
}

// ---------------------------------------------------------------------------
// Unit tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -- Helper tests ---------------------------------------------------------

    #[test]
    fn test_civil_from_days_epoch() {
        // 1970-01-01 is day 0
        let (y, m, d) = civil_from_days(0);
        assert_eq!((y, m, d), (1970, 1, 1));
    }

    #[test]
    fn test_civil_from_days_known_date() {
        // 2000-01-01 is day 10957
        let (y, m, d) = civil_from_days(10957);
        assert_eq!((y, m, d), (2000, 1, 1));
    }

    #[test]
    fn test_civil_from_days_leap_day() {
        // 2000-02-29 is day 10957 + 31 + 28 = 11016
        // Jan has 31 days, Feb 29th is the 60th day of 2000.
        // day 10957 = Jan 1, day 10957+59 = Feb 29
        let (y, m, d) = civil_from_days(10957 + 59);
        assert_eq!((y, m, d), (2000, 2, 29));
    }

    #[test]
    fn test_civil_from_days_negative() {
        // 1969-12-31 is day -1
        let (y, m, d) = civil_from_days(-1);
        assert_eq!((y, m, d), (1969, 12, 31));
    }

    #[test]
    fn test_is_leap_year() {
        assert!(is_leap_year(2000));
        assert!(is_leap_year(2024));
        assert!(!is_leap_year(1900));
        assert!(!is_leap_year(2023));
    }

    #[test]
    fn test_day_of_year() {
        assert_eq!(day_of_year(2023, 1, 1), 1);
        assert_eq!(day_of_year(2023, 12, 31), 365);
        assert_eq!(day_of_year(2024, 12, 31), 366); // leap year
        assert_eq!(day_of_year(2024, 3, 1), 61); // leap year: 31 + 29 + 1
    }

    #[test]
    fn test_day_of_week_from_days() {
        // 1970-01-01 (day 0) was Thursday = ISO 4
        assert_eq!(day_of_week_from_days(0), 4);
        // 1970-01-05 (day 4) was Monday = ISO 1
        assert_eq!(day_of_week_from_days(4), 1);
        // 1970-01-04 (day 3) was Sunday = ISO 7
        assert_eq!(day_of_week_from_days(3), 7);
    }

    #[test]
    fn test_write_to_dest_exact() {
        let mut buf = [0u8; 6];
        // SAFETY: buf is valid for 6 writable bytes.
        unsafe { write_to_dest(buf.as_mut_ptr(), 6, b"260218") };
        assert_eq!(&buf, b"260218");
    }

    #[test]
    fn test_write_to_dest_truncate() {
        let mut buf = [0u8; 4];
        // SAFETY: buf is valid for 4 writable bytes.
        unsafe { write_to_dest(buf.as_mut_ptr(), 4, b"260218") };
        assert_eq!(&buf, b"2602");
    }

    #[test]
    fn test_write_to_dest_pad() {
        let mut buf = [0u8; 10];
        // SAFETY: buf is valid for 10 writable bytes.
        unsafe { write_to_dest(buf.as_mut_ptr(), 10, b"260218") };
        assert_eq!(&buf, b"260218    ");
    }

    // -- ACCEPT function tests ------------------------------------------------

    #[test]
    fn test_accept_date_format() {
        let mut buf = [0u8; 6];
        // SAFETY: buf is valid for 6 writable bytes.
        unsafe { cobolrt_accept_date(buf.as_mut_ptr(), 6) };
        // All 6 bytes should be ASCII digits
        for &b in &buf {
            assert!(b.is_ascii_digit(), "non-digit byte: {}", b);
        }
        // Month should be 01..12
        let mm = (buf[2] - b'0') * 10 + (buf[3] - b'0');
        assert!((1..=12).contains(&mm), "month out of range: {}", mm);
        // Day should be 01..31
        let dd = (buf[4] - b'0') * 10 + (buf[5] - b'0');
        assert!((1..=31).contains(&dd), "day out of range: {}", dd);
    }

    #[test]
    fn test_accept_date_truncation() {
        let mut buf = [0u8; 4];
        // SAFETY: buf is valid for 4 writable bytes.
        unsafe { cobolrt_accept_date(buf.as_mut_ptr(), 4) };
        // Should get first 4 digits (YYMM)
        for &b in &buf {
            assert!(b.is_ascii_digit(), "non-digit byte: {}", b);
        }
    }

    #[test]
    fn test_accept_date_padding() {
        let mut buf = [0u8; 10];
        // SAFETY: buf is valid for 10 writable bytes.
        unsafe { cobolrt_accept_date(buf.as_mut_ptr(), 10) };
        // First 6 bytes: digits. Last 4: spaces.
        for &b in &buf[..6] {
            assert!(b.is_ascii_digit(), "non-digit byte: {}", b);
        }
        for &b in &buf[6..] {
            assert_eq!(b, b' ', "expected space padding");
        }
    }

    #[test]
    fn test_accept_date_null() {
        // SAFETY: Null pointer is handled gracefully by the function.
        unsafe { cobolrt_accept_date(std::ptr::null_mut(), 6) }; // should not panic
    }

    #[test]
    fn test_accept_day_format() {
        let mut buf = [0u8; 5];
        // SAFETY: buf is valid for 5 writable bytes.
        unsafe { cobolrt_accept_day(buf.as_mut_ptr(), 5) };
        // All 5 bytes should be ASCII digits
        for &b in &buf {
            assert!(b.is_ascii_digit(), "non-digit byte: {}", b);
        }
        // Day-of-year should be 001..366
        let ddd =
            (buf[2] - b'0') as u32 * 100 + (buf[3] - b'0') as u32 * 10 + (buf[4] - b'0') as u32;
        assert!(
            (1..=366).contains(&ddd),
            "day-of-year out of range: {}",
            ddd
        );
    }

    #[test]
    fn test_accept_day_null() {
        // SAFETY: Null pointer is handled gracefully by the function.
        unsafe { cobolrt_accept_day(std::ptr::null_mut(), 5) }; // should not panic
    }

    #[test]
    fn test_accept_time_format() {
        let mut buf = [0u8; 8];
        // SAFETY: buf is valid for 8 writable bytes.
        unsafe { cobolrt_accept_time(buf.as_mut_ptr(), 8) };
        // All 8 bytes should be ASCII digits
        for &b in &buf {
            assert!(b.is_ascii_digit(), "non-digit byte: {}", b);
        }
        // Hours should be 00..23
        let hh = (buf[0] - b'0') * 10 + (buf[1] - b'0');
        assert!(hh <= 23, "hours out of range: {}", hh);
        // Minutes should be 00..59
        let mm = (buf[2] - b'0') * 10 + (buf[3] - b'0');
        assert!(mm <= 59, "minutes out of range: {}", mm);
        // Seconds should be 00..59
        let ss = (buf[4] - b'0') * 10 + (buf[5] - b'0');
        assert!(ss <= 59, "seconds out of range: {}", ss);
        // Hundredths should be 00..99
        let cs = (buf[6] - b'0') * 10 + (buf[7] - b'0');
        assert!(cs <= 99, "hundredths out of range: {}", cs);
    }

    #[test]
    fn test_accept_time_null() {
        // SAFETY: Null pointer is handled gracefully by the function.
        unsafe { cobolrt_accept_time(std::ptr::null_mut(), 8) }; // should not panic
    }

    #[test]
    fn test_accept_day_of_week_format() {
        let mut buf = [0u8; 1];
        // SAFETY: buf is valid for 1 writable byte.
        unsafe { cobolrt_accept_day_of_week(buf.as_mut_ptr(), 1) };
        // Should be '1'..'7'
        assert!(
            (b'1'..=b'7').contains(&buf[0]),
            "day-of-week out of range: {}",
            buf[0]
        );
    }

    #[test]
    fn test_accept_day_of_week_padding() {
        let mut buf = [0u8; 5];
        // SAFETY: buf is valid for 5 writable bytes.
        unsafe { cobolrt_accept_day_of_week(buf.as_mut_ptr(), 5) };
        // First byte: digit 1-7. Rest: spaces.
        assert!((b'1'..=b'7').contains(&buf[0]));
        for &b in &buf[1..] {
            assert_eq!(b, b' ', "expected space padding");
        }
    }

    #[test]
    fn test_accept_day_of_week_null() {
        // SAFETY: Null pointer is handled gracefully by the function.
        unsafe { cobolrt_accept_day_of_week(std::ptr::null_mut(), 1) }; // should not panic
    }
}
