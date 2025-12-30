use std::{fmt, time::Duration};

#[derive(Debug, Clone)]
pub struct MetricErased {
    fmt: fn(&mut fmt::Formatter, f64) -> fmt::Result,
    name: String,
}

impl MetricErased {
    pub(crate) fn erase<T: Metric>(metric: T) -> MetricErased {
        MetricErased {
            fmt: |f, v| T::fmt(f, T::from_f64(v)),
            name: metric.name().into(),
        }
    }
    pub fn display(&self, v: f64) -> impl fmt::Display {
        struct Printer {
            f: fn(&mut fmt::Formatter, f64) -> fmt::Result,
            v: f64,
        }

        impl fmt::Display for Printer {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                (self.f)(f, self.v)
            }
        }

        Printer { f: self.fmt, v }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

pub trait Metric: 'static {
    type Measurement: Clone + 'static;
    fn to_f64(v: Self::Measurement) -> f64;
    fn from_f64(v: f64) -> Self::Measurement;

    fn name(&self) -> impl Into<String>;
    fn fmt(f: &mut fmt::Formatter, v: Self::Measurement) -> fmt::Result;
}

#[derive(Debug, Clone, Copy)]
pub struct Time;

impl Metric for Time {
    type Measurement = Duration;

    fn to_f64(v: Duration) -> f64 {
        v.as_nanos() as f64
    }

    fn from_f64(v: f64) -> Duration {
        let nanos = v as u128;
        Duration::new(
            (nanos / 1_000_000_000) as u64,
            (nanos % 1_000_000_000) as u32,
        )
    }

    fn name(&self) -> impl Into<String> {
        "time"
    }
    fn fmt(f: &mut fmt::Formatter, v: Duration) -> fmt::Result {
        write!(f, "{v:.2?}")
    }
}
