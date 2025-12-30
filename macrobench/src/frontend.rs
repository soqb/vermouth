use std::{fmt, future::Future};

use crate::backend::Backend;

#[cfg(feature = "tui")]
pub mod tui;

/// A frontend configuration capable of surfacing benchmark results to the user.
pub trait Frontend {
    /// Drives the frontend until completion.
    ///
    /// This method takes ownership of the frontend.
    fn lifecycle(self, backend: &mut Backend) -> impl Future<Output = anyhow::Result<()>>;
}

/// A utility struct for printing trees.
#[derive(Debug, Default, Clone, Copy)]
pub struct Treamble {
    bits: u128,
    depth: usize,
    is_final: Option<bool>,
}

impl Treamble {
    fn nest(self, is_final: bool) -> Treamble {
        let Self {
            bits,
            depth,
            is_final: _,
        } = self;
        let bit: u128 = match self.is_final {
            Some(true) => 0,
            Some(false) => 1,
            None => {
                return Treamble {
                    bits,
                    depth,
                    is_final: Some(is_final),
                };
            }
        };

        Treamble {
            bits: bits | (bit << depth),
            depth: depth + 1,
            is_final: Some(is_final),
        }
    }
}

impl fmt::Display for Treamble {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for m in (0..self.depth).map(|i| 1 << i) {
            let s = (self.bits & m) > 0;
            let s = if s { "│ " } else { "  " };
            f.write_str(s)?;
        }

        let Some(end) = self.is_final else {
            return Ok(());
        };
        let s = if end { "└─" } else { "├─" };
        f.write_str(s)
    }
}

/// The no-op backend.
pub struct Nop;

impl Frontend for Nop {
    async fn lifecycle(self, _: &mut Backend) -> anyhow::Result<()> {
        Ok(())
    }
}

pub fn default_frontend() -> impl Frontend {
    #[cfg(feature = "tui")]
    return tui::Tui {};
    #[cfg(not(feature = "tui"))]
    return Nop;
}
