use std::io::Write;

/// Display a null-terminated string to stdout (DISPLAY ... UPON CONSOLE)
#[no_mangle]
pub extern "C" fn cobolrt_display(data: *const u8, len: u32) {
    if data.is_null() {
        return;
    }
    let slice = unsafe { std::slice::from_raw_parts(data, len as usize) };
    let _ = std::io::stdout().write_all(slice);
}

/// Display a string followed by a newline
#[no_mangle]
pub extern "C" fn cobolrt_display_line(data: *const u8, len: u32) {
    if data.is_null() {
        println!();
        return;
    }
    let slice = unsafe { std::slice::from_raw_parts(data, len as usize) };
    let _ = std::io::stdout().write_all(slice);
    let _ = std::io::stdout().write_all(b"\n");
    let _ = std::io::stdout().flush();
}

/// Display a numeric value formatted according to PIC
#[no_mangle]
pub extern "C" fn cobolrt_display_numeric(
    data: *const u8,
    len: u32,
    pic_digits: u16,
    pic_scale: i16,
) {
    let _ = (data, len, pic_digits, pic_scale);
    // TODO: implement numeric display formatting
}

/// ACCEPT FROM CONSOLE - read a line from stdin
#[no_mangle]
pub extern "C" fn cobolrt_accept(buffer: *mut u8, buffer_len: u32) -> u32 {
    if buffer.is_null() {
        return 0;
    }
    let mut input = String::new();
    match std::io::stdin().read_line(&mut input) {
        Ok(_) => {
            let input = input.trim_end_matches('\n').trim_end_matches('\r');
            let bytes = input.as_bytes();
            let copy_len = std::cmp::min(bytes.len(), buffer_len as usize);
            unsafe {
                std::ptr::copy_nonoverlapping(bytes.as_ptr(), buffer, copy_len);
                // Pad with spaces (COBOL standard)
                if copy_len < buffer_len as usize {
                    std::ptr::write_bytes(
                        buffer.add(copy_len),
                        b' ',
                        buffer_len as usize - copy_len,
                    );
                }
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
