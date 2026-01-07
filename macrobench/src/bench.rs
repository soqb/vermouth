use std::{marker::PhantomData, time::Instant};

use smol::channel;

use crate::{Metric, MetricErased, Time, backend, db};

pub trait Sample<M: Metric> {
    fn sample(&mut self) -> M;
}

impl<F: ?Sized, M: Metric> Sample<M> for F
where
    F: FnMut() -> M,
{
    fn sample(&mut self) -> M {
        self()
    }
}

impl db::DbSender for channel::Sender<backend::Message> {
    fn send(&self, m: db::DbMessage) {
        self.try_send(m.into()).unwrap();
    }
}

/// The root for benchmark running.
///
/// See [Bencher::group].
pub struct Bencher {
    sock: db::DbSocket<channel::Sender<backend::Message>>,
}

impl Bencher {
    pub(crate) fn from_tx(tx: channel::Sender<backend::Message>) -> Bencher {
        Bencher {
            sock: db::DbSocket::new(tx),
        }
    }

    /// Creates a [`BenchGroup`] for clustering benchmarks.
    ///
    /// Note that [`BenchGroup`] has its own [`group`](BenchGroup::group) method;
    /// benchmarks are the leaves on a tree of groups.
    pub fn group(&self, name: impl Into<String>) -> BenchGroup<'_> {
        BenchGroup {
            db_addr: self.sock.alloc_group(db::Group {
                name: name.into(),
                parent: None,
            }),
            bencher: self,
        }
    }
}

pub struct Bench<'a, T> {
    bencher: &'a Bencher,
    db_addr: db::BenchAddr,
    _marker: PhantomData<T>,
}

impl<'a, T> Drop for Bench<'a, T> {
    fn drop(&mut self) {
        self.bencher.sock.freeze_bench(self.db_addr);
    }
}

pub struct BenchGroup<'a> {
    bencher: &'a Bencher,
    db_addr: db::GroupAddr,
}

impl<'a> BenchGroup<'a> {
    pub fn group(&self, name: impl Into<String>) -> BenchGroup<'a> {
        BenchGroup {
            db_addr: self.bencher.sock.alloc_group(db::Group {
                name: name.into(),
                parent: Some(self.db_addr),
            }),
            bencher: self.bencher,
        }
    }

    pub fn bench<T: Metric>(&self, name: impl Into<String>, metric: T) -> Bench<'a, T> {
        Bench {
            bencher: self.bencher,
            db_addr: self.bencher.sock.alloc_bench(db::Bench {
                name: name.into(),
                parent: self.db_addr,
                metric: MetricErased::erase(metric),
            }),
            _marker: PhantomData,
        }
    }
}

impl<'a, T: Metric> Bench<'a, T> {
    pub fn warm(&self) {
        self.bencher.sock.warmup_bench(self.db_addr);
    }

    pub fn mark(&self, mk: &Marker, measurement: T::Measurement) {
        let _ = mk;
        let sample = vec![T::to_f64(measurement)];
        self.bencher.sock.publish_sample(self.db_addr, sample);
    }
}

pub struct Marker {
    _marker: PhantomData<()>,
}

pub struct MarkEnv {
    pub n: usize,
}
impl MarkEnv {
    #[inline(never)]
    pub fn measure<T, F, W, M>(self, mut measure: F, warm: W, report: M)
    where
        F: FnMut() -> T,
        W: Fn(),
        M: Fn(&Marker, T),
    {
        // warmup.
        let _ = measure();
        warm();

        for _ in 0..self.n {
            let m = measure();
            let mk = Marker {
                _marker: PhantomData,
            };
            report(&mk, m);
        }
    }

    pub fn measure_wall_time<F, M>(self, mut measure: F, bench: &Bench<Time>)
    where
        F: FnMut(),
    {
        self.measure(
            move || {
                let start = Instant::now();
                measure();
                start.elapsed()
            },
            || bench.warm(),
            |mk, m| bench.mark(mk, m),
        );
    }
}
