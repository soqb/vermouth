use std::iter::Sum;

#[derive(Debug, Clone, Copy)]
pub struct Snapshot {
    pub n: u64,
    pub mean: f64,
    pub biased_sample_variance: f64,
    pub unbiased_sample_variance: f64,
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Wip {
    n: f64,
    mean: f64,
    sdm: f64,
}

impl Wip {
    pub fn n(&self) -> u64 {
        self.n as u64
    }

    pub fn from_sample(xs: &[f64]) -> Wip {
        let n = xs.len() as f64;
        let sum = f64::sum(xs.iter());
        let squared_sum = f64::sum(xs.iter().map(|x| x * x));
        let mean = sum / n;
        let sdm = squared_sum - (sum * sum) / n;

        Wip { n, mean, sdm }
    }

    pub fn chan_combine(self, b: Wip) -> Wip {
        let a = self;
        let n = a.n + b.n;
        let delta = b.mean - a.mean;
        let mean = a.mean + delta * (b.n / n);
        let sdm = a.sdm + b.sdm + (delta * delta * a.n * b.n) / n;

        Wip { n, mean, sdm }
    }

    pub fn to_snapshot(self) -> Snapshot {
        let Wip { n, mean, sdm } = self;
        Snapshot {
            mean,
            biased_sample_variance: sdm / n,
            unbiased_sample_variance: sdm / (n - 1.),
            n: self.n(),
        }
    }

    pub fn update(&mut self, xs: &[f64]) {
        *self = self.chan_combine(Wip::from_sample(xs));
    }
}
