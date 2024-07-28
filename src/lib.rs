#![allow(clippy::toplevel_ref_arg)]
//! A new kind of parser for procedural macros.
//!
//! As opposed to [`syn`],
//! this crate is designed around the philosophy that malformed input
//! should be handled gracefully by procedural macros.
//!
//! See the methods on the [`Parser`] type for structural documentation.
//!
//! [`syn`]: https://crates.io/crates/syn

#[cfg(not(feature = "proc-macro2"))]
extern crate proc_macro;
#[cfg(feature = "proc-macro2")]
extern crate proc_macro2 as proc_macro;

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_declare_test {
    () => {
        #[cfg(not(feature = "proc-macro2"))]
        compile_error!(
            "tests must be run with the `proc-macro2` feature.\n\
            `proc-macro` doesn't support execution outside the rustc harness"
        );
        #[cfg(feature = "proc-macro2")]
        extern crate proc_macro2 as proc_macro;
    };
}

mod error;
mod parser;
mod span;
mod tokens;

pub use self::{error::*, parser::*, span::*, tokens::*};

#[cfg(feature = "attributes")]
pub mod attributes;
