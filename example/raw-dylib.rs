fn main() {
    #[cfg(all(target_arch = "x86_64", target_os = "windows", target_env = "msvc"))]
    x86_64_pc_windows_msvc::test();
}

#[cfg(all(target_arch = "x86_64", target_os = "windows", target_env = "msvc"))]
mod x86_64_pc_windows_msvc {
    #![allow(clippy::upper_case_acronyms)]

    // Expanded windows_sys, with --cfg windows_raw_dylib, on not(target_arch = "x86").
    //
    // With target_arch = "x86", #[link] needs import_name_type = "undecorated" for windows APIs for
    // windows APIs - and the extern abi depends on the specific API.

    // use windows_sys::core::PWSTR;
    // use windows_sys::{Win32::Foundation::*, Win32::UI::WindowsAndMessaging::*};
    type PWSTR = *mut u16;
    type BOOL = i32;
    type HWND = isize;
    type LPARAM = isize;
    type WNDENUMPROC = Option<unsafe extern "system" fn(hwnd: HWND, param: LPARAM) -> BOOL>;

    #[link(name = "user32.dll", kind = "raw-dylib", modifiers = "+verbatim")]
    extern "system" {
        fn EnumWindows(lpenumfunc: WNDENUMPROC, lparam: LPARAM) -> BOOL;

        fn GetWindowTextW(hwnd: HWND, buf: PWSTR, buflen: i32) -> i32;
    }

    pub fn test() {
        unsafe { EnumWindows(Some(enum_window), 0) };
    }

    extern "system" fn enum_window(window: HWND, _: LPARAM) -> BOOL {
        let mut text: [u16; 512] = [0; 512];

        let len = unsafe { GetWindowTextW(window, text.as_mut_ptr(), text.len() as i32) };
        let text = String::from_utf16_lossy(&text[..len as usize]);

        1 // TRUE
    }
}
