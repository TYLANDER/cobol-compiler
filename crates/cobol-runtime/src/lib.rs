pub mod arithmetic;
pub mod compare;
pub mod decimal;
pub mod display;
pub mod file_io;
pub mod inspect;
pub mod intrinsics;
pub mod memory;
pub mod perform;
pub mod sort;

#[cfg(test)]
mod tests {
    #[test]
    fn runtime_modules_exist() {
        // Verify that all modules are accessible
        let _ = std::mem::size_of::<crate::decimal::PackedDecimal>();
        let _ = std::mem::size_of::<crate::file_io::FileHandle>();
    }

    #[test]
    fn display_line_does_not_panic_on_null() {
        // SAFETY: Passing null pointer - the function handles null gracefully.
        unsafe {
            crate::display::cobolrt_display_line(std::ptr::null(), 0);
        }
    }

    #[test]
    fn display_does_not_panic_on_null() {
        // SAFETY: Passing null pointer - the function handles null gracefully.
        unsafe {
            crate::display::cobolrt_display(std::ptr::null(), 0);
        }
    }

    #[test]
    fn memory_alloc_and_free() {
        let ptr = crate::memory::cobolrt_alloc(128);
        assert!(!ptr.is_null());
        // Verify it's zero-initialized
        // SAFETY: `ptr` was just returned by cobolrt_alloc with size 128.
        let slice = unsafe { std::slice::from_raw_parts(ptr, 128) };
        assert!(slice.iter().all(|&b| b == 0));
        // SAFETY: `ptr` was allocated by cobolrt_alloc with size 128.
        unsafe {
            crate::memory::cobolrt_free(ptr, 128);
        }
    }

    #[test]
    fn memory_move_alphanumeric_padding() {
        let src = b"HELLO";
        let mut dest = [0u8; 10];
        // SAFETY: `src` and `dest` point to valid, non-overlapping memory of the given lengths.
        unsafe {
            crate::memory::cobolrt_move_alphanumeric(src.as_ptr(), 5, dest.as_mut_ptr(), 10);
        }
        assert_eq!(&dest, b"HELLO     ");
    }

    #[test]
    fn memory_move_alphanumeric_truncation() {
        let src = b"HELLO WORLD";
        let mut dest = [0u8; 5];
        // SAFETY: `src` and `dest` point to valid, non-overlapping memory of the given lengths.
        unsafe {
            crate::memory::cobolrt_move_alphanumeric(src.as_ptr(), 11, dest.as_mut_ptr(), 5);
        }
        assert_eq!(&dest, b"HELLO");
    }

    #[test]
    fn initialize_alphanumeric_fills_spaces() {
        let mut buf = [0u8; 8];
        // SAFETY: `buf` points to 8 writable bytes.
        unsafe {
            crate::memory::cobolrt_initialize_alphanumeric(buf.as_mut_ptr(), 8);
        }
        assert_eq!(&buf, b"        ");
    }

    #[test]
    fn initialize_numeric_fills_zeros() {
        let mut buf = [0u8; 5];
        // SAFETY: `buf` points to 5 writable bytes.
        unsafe {
            crate::memory::cobolrt_initialize_numeric(buf.as_mut_ptr(), 5);
        }
        assert_eq!(&buf, b"00000");
    }

    #[test]
    fn perform_stack_push_pop() {
        crate::perform::cobolrt_perform_push(42);
        crate::perform::cobolrt_perform_push(99);
        assert_eq!(crate::perform::cobolrt_perform_depth(), 2);
        assert_eq!(crate::perform::cobolrt_perform_pop(), 99);
        assert_eq!(crate::perform::cobolrt_perform_pop(), 42);
        assert_eq!(crate::perform::cobolrt_perform_depth(), 0);
    }

    #[test]
    fn perform_stack_pop_empty_returns_zero() {
        // Ensure stack is empty first (thread-local may have leftover state)
        while crate::perform::cobolrt_perform_pop() != 0 {}
        assert_eq!(crate::perform::cobolrt_perform_pop(), 0);
    }

    #[test]
    fn intrinsic_length() {
        // SAFETY: Passing null pointer - the function ignores the data pointer.
        unsafe {
            assert_eq!(
                crate::intrinsics::cobolrt_intrinsic_length(std::ptr::null(), 42),
                42
            );
        }
    }

    #[test]
    fn intrinsic_reverse() {
        let input = b"ABCDE";
        let mut output = [0u8; 5];
        // SAFETY: `input` and `output` are valid, non-overlapping, and have at least 5 bytes.
        unsafe {
            crate::intrinsics::cobolrt_intrinsic_reverse(input.as_ptr(), 5, output.as_mut_ptr());
        }
        assert_eq!(&output, b"EDCBA");
    }

    #[test]
    fn intrinsic_upper_case() {
        let mut data = *b"hello world";
        // SAFETY: `data` points to 11 writable bytes.
        unsafe {
            crate::intrinsics::cobolrt_intrinsic_upper_case(data.as_mut_ptr(), 11);
        }
        assert_eq!(&data, b"HELLO WORLD");
    }

    #[test]
    fn intrinsic_lower_case() {
        let mut data = *b"HELLO WORLD";
        // SAFETY: `data` points to 11 writable bytes.
        unsafe {
            crate::intrinsics::cobolrt_intrinsic_lower_case(data.as_mut_ptr(), 11);
        }
        assert_eq!(&data, b"hello world");
    }

    #[test]
    fn intrinsic_mod() {
        assert_eq!(crate::intrinsics::cobolrt_intrinsic_mod(10, 3), 1);
        assert_eq!(crate::intrinsics::cobolrt_intrinsic_mod(10, 0), 0);
    }

    #[test]
    fn intrinsic_integer() {
        assert_eq!(crate::intrinsics::cobolrt_intrinsic_integer(3.7), 3);
        assert_eq!(crate::intrinsics::cobolrt_intrinsic_integer(-3.7), -4);
    }

    #[test]
    fn intrinsic_integer_part() {
        assert_eq!(crate::intrinsics::cobolrt_intrinsic_integer_part(3.7), 3);
        assert_eq!(crate::intrinsics::cobolrt_intrinsic_integer_part(-3.7), -3);
    }

    #[test]
    fn intrinsic_ord_and_char() {
        assert_eq!(crate::intrinsics::cobolrt_intrinsic_ord(b'A'), 66);
        assert_eq!(crate::intrinsics::cobolrt_intrinsic_char(66), b'A');
    }

    #[test]
    fn decimal_cmp_stub() {
        // SAFETY: Passing null pointers - the function handles null gracefully.
        unsafe {
            assert_eq!(
                crate::decimal::cobolrt_decimal_cmp(std::ptr::null(), std::ptr::null()),
                0,
            );
        }
    }

    #[test]
    fn decimal_div_null_returns_error() {
        // SAFETY: Passing null pointers - the function returns -1 for null inputs.
        unsafe {
            assert_eq!(
                crate::decimal::cobolrt_decimal_div(
                    std::ptr::null(),
                    std::ptr::null(),
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                    false,
                ),
                -1,
            );
        }
    }

    #[test]
    fn file_read_returns_at_end() {
        // SAFETY: Passing null pointers - the function is a stub that ignores its arguments.
        unsafe {
            assert_eq!(
                crate::file_io::cobolrt_file_read(std::ptr::null_mut(), std::ptr::null_mut(), 0),
                10,
            );
        }
    }

    #[test]
    fn sort_return_returns_end() {
        // SAFETY: Passing null pointers - the function is a stub that ignores its arguments.
        unsafe {
            assert_eq!(
                crate::sort::cobolrt_sort_return(std::ptr::null_mut(), std::ptr::null_mut(), 0),
                10,
            );
        }
    }
}
