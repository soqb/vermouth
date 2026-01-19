mod bench;
pub mod db;
mod metric;
#[cfg(feature = "rustc-timings")]
pub mod rustc;

pub mod backend;
pub mod frontend;
pub mod stats;

pub use self::{bench::*, metric::*};

use smol::channel;

use std::{panic::catch_unwind, thread};

pub fn main_with<R, F: frontend::Frontend + 'static>(
    name: impl Into<String>,
    bench_main: fn(&Bencher) -> R,
    get_frontend: fn() -> F,
) -> R {
    let name = name.into();
    let (tx, rx) = channel::unbounded();
    let thread_frontend = thread::spawn(move || {
        let mut backend = backend::Backend::new(name, rx);

        smol::block_on(async {
            frontend::Frontend::lifecycle(get_frontend(), &mut backend).await?;
            backend.until_done().await;
            anyhow::Ok(())
        })
    });

    let r = match catch_unwind(|| {
        let bencher = Bencher::from_tx(tx);
        bench_main(&bencher)
    }) {
        Ok(r) => r,
        #[cfg(feature = "rustc-timings")]
        Err(payload) => {
            if let Some(x) = payload.downcast_ref() {
                rustc::ProcessFailed::report(x).unwrap();
            }

            if let Some(x) = payload.downcast_ref() {
                rustc::JobError::report(x).unwrap();
            }

            std::panic::panic_any(payload);
        }
        #[cfg(not(feature = "rustc-timings"))]
        Err(payload) => std::panic::panic_any(payload),
    };

    thread_frontend.join().unwrap().unwrap();

    r
}

/// The entrypoint for benchmarking.
///
///
pub fn main<R>(name: impl Into<String>, bench_main: fn(&Bencher) -> R) -> R {
    main_with(name, bench_main, frontend::default_frontend)
}
