use std::ffi::{CStr, CString};

pub fn cstr(s: &str) -> Box<CStr> {
    CString::new(s).unwrap().into_boxed_c_str()
}
