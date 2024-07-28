#![allow(clippy::toplevel_ref_arg)]
//! A new kind of parser for procedural macros.
//!
//! See the methods on the [`Parser`] type for structural documentation.

extern crate proc_macro;

mod error;
mod parser;
mod span;
mod tokens;

pub use self::{error::*, parser::*, span::*, tokens::*};

#[cfg(feature = "attributes")]
pub mod attributes;
