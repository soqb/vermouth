use std::{
    ffi::OsString,
    fmt, fs,
    io::{self, Write},
    iter::Sum,
    ops::{Add, AddAssign},
    path::{Path, PathBuf},
    process,
    time::Duration,
};

#[derive(Debug, Clone, Copy)]
pub enum Scenario {
    Fulldeps,
    Incremental,
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct Target {
    /// `rustc` flags passed to the _final_ compiler invocation.
    pub rustflags: Vec<OsString>,
    pub features: Vec<String>,
    pub manifest_path: OsString,
    pub bin_target: Option<OsString>,
    pub default_features: bool,
    pub scenario: Scenario,
}

#[derive(Default, Debug, Clone)]
#[non_exhaustive]
pub struct JobCfg {
    /// `rustc` flags passed to all compiler invocations.
    pub rustflags: Option<OsString>,
}

pub struct Job<'a> {
    spec: &'a Target,
    cfg: &'a JobCfg,
    target_dir: PathBuf,
}

#[derive(Debug)]
pub enum ProcessFailed {
    NonZeroExit(process::Output),
    Io(io::Error),
}

impl From<io::Error> for ProcessFailed {
    fn from(value: io::Error) -> Self {
        ProcessFailed::Io(value)
    }
}

impl ProcessFailed {
    pub fn report(&self) -> io::Result<()> {
        match self {
            ProcessFailed::NonZeroExit(output) => {
                let mut err = io::stderr().lock();

                write!(err, "job failed with {}\n== STDOUT ==\n", output.status)?;
                err.write_all(&output.stdout)?;
                write!(err, "== STDOUT ==\n")?;

                write!(err, "\n== STDERR ==\n")?;
                err.write_all(&output.stderr)?;
                write!(err, "== STDERR ==\n")?;

                err.flush()?;
            }
            ProcessFailed::Io(err) => eprintln!("{err}"),
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum JobError {
    Io(io::Error),
    Failed(ProcessFailed),
    Json(facet_json::JsonError),
    MissingMeasurement(String),
}

impl JobError {
    pub fn report(&self) -> io::Result<()> {
        match self {
            JobError::Failed(pf) => pf.report(),
            _ => {
                eprintln!("job failed: {self}");
                Ok(())
            }
        }
    }
}

impl fmt::Display for JobError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JobError::Io(error) => fmt::Display::fmt(error, f),
            JobError::Failed(_) => {
                write!(f, "job failed")
            }
            JobError::Json(error) => fmt::Display::fmt(error, f),
            JobError::MissingMeasurement(s) => write!(f, "missing measurement: {s}"),
        }
    }
}

impl From<io::Error> for JobError {
    fn from(value: io::Error) -> Self {
        JobError::Io(value)
    }
}

impl From<facet_json::JsonError> for JobError {
    fn from(value: facet_json::JsonError) -> Self {
        JobError::Json(value)
    }
}

impl Target {
    pub fn from_project(manifest_path: impl Into<OsString>, scenario: Scenario) -> Target {
        Target {
            rustflags: Vec::new(),
            features: Vec::new(),
            manifest_path: manifest_path.into(),
            bin_target: None,
            default_features: true,
            scenario,
        }
    }

    pub fn setup(&self) -> Result<(), ProcessFailed> {
        let mut cmd = process::Command::new("cargo");
        cmd.args(["+nightly", "fetch", "--color=always", "--manifest-path"])
            .arg(&self.manifest_path)
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::piped());

        let output = cmd.spawn()?.wait_with_output()?;

        if output.status.success() {
            Ok(())
        } else {
            Err(ProcessFailed::NonZeroExit(output))
        }
    }

    pub fn new_job<'a>(
        &'a self,
        cfg: &'a JobCfg,
        target_base: impl AsRef<Path>,
        uid: u64,
    ) -> Job<'a> {
        // FIXME: i don't like this..
        let target_dir: PathBuf = target_base.as_ref().join(&uid.to_string());
        Job {
            spec: self,
            target_dir,
            cfg,
        }
    }
}

impl<'a> Job<'a> {
    fn kill_target_dir(&self) -> io::Result<()> {
        fs::remove_dir_all(&self.target_dir).or_else(|err| {
            if err.kind() == io::ErrorKind::NotFound {
                Ok(())
            } else {
                Err(err)
            }
        })
    }

    fn command_for_build(&self) -> process::Command {
        let mut cmd = process::Command::new("cargo");
        cmd.args([
            "+nightly",
            "rustc",
            "--color=always",
            "--frozen",
            "-Zunstable-options",
            "--timings=json",
            "--manifest-path",
        ])
        .arg(&self.spec.manifest_path)
        .arg("--target-dir")
        .arg(&self.target_dir)
        .args(self.spec.features.iter().flat_map(|f| ["-F", f]));
        if !self.spec.default_features {
            cmd.arg("--no-default-features");
        }

        if let Some(bin) = self.spec.bin_target.as_ref() {
            cmd.arg("--bin").arg(bin);
        }

        cmd.args(["--", "-Zunstable-options", "--json=timings"])
            .args(&self.spec.rustflags);

        if let Some(flags) = self.cfg.rustflags.as_ref() {
            cmd.env("RUSTFLAGS", flags);
        }

        cmd.stdout(process::Stdio::piped())
            .stderr(process::Stdio::piped());

        cmd
    }

    fn cleanup(&self) -> Result<(), JobError> {
        self.kill_target_dir()?;
        Ok(())
    }

    fn exec(&self) -> Result<JobTimings, JobError> {
        self.kill_target_dir()?;
        let output = self.command_for_build().spawn()?.wait_with_output()?;

        if !output.status.success() {
            return Err(JobError::Failed(ProcessFailed::NonZeroExit(output)));
        }

        let list: Vec<(json::Target, Timings)> = output
            .stdout
            .split(|&b| b == b'\n')
            .filter_map(|line| Timings::parse_from_json(line).transpose())
            .collect::<Result<_, JobError>>()?;

        Ok(JobTimings { list })

        // measure_timings_from_json(
        //     timings.iter(),
        //     &self.spec.package_to_measure,
        //     self.cfg.scenario,
        // )
        // .ok_or_else(|| JobError::MissingMeasurement(self.spec.package_to_measure.clone()))
    }

    pub fn try_run(self) -> Result<JobTimings, JobError> {
        self.exec()
            .inspect_err(|_| _ = self.cleanup())
            .and_then(|jt| {
                self.cleanup()?;
                Ok(jt)
            })
    }

    pub fn run(self) -> JobTimings {
        match self.try_run() {
            Ok(jt) => jt,
            Err(err) => std::panic::panic_any(err),
        }
        // Timings::default()
    }
}

pub mod json {
    #[derive(Debug, Clone, facet::Facet)]
    pub struct Target {
        pub name: String,
    }
    #[derive(Debug, Clone, facet::Facet)]
    pub struct UnitTime {
        pub target: Target,
        pub duration: f64,
        pub rmeta_time: Option<f64>,
    }
}

#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub enum Timing {
    /// Total compilation time, the most reliable metric.
    CompileTime,
    /// Time spent in the frontend, best effort.
    FrontendTime,
    /// Time spent in the backend, best effort.
    BackendTime,
}

#[derive(Debug, Clone, Default)]
pub struct JobTimings {
    list: Vec<(json::Target, Timings)>,
}

impl JobTimings {
    pub fn such_that<'a>(
        &'a self,
        mut p: impl FnMut(&json::Target) -> bool + 'a,
    ) -> impl Iterator<Item = Timings> + 'a {
        self.list
            .iter()
            .filter(move |&(tg, _)| p(tg))
            .map(|(_, tm)| tm.clone())
    }
}

#[derive(Debug, Clone, Default)]
pub struct Timings {
    /// Total compilation time.
    total: Duration,
    /// Time known to have been spent in the frontend (<= `total`).
    certainly_frontend: Duration,
    /// Time known to have been spent in the backend (<= `total`).
    certainly_backend: Duration,
}

impl AddAssign for Timings {
    fn add_assign(&mut self, rhs: Timings) {
        self.total += rhs.total;
        self.certainly_frontend += rhs.certainly_frontend;
        self.certainly_backend += rhs.certainly_backend;
    }
}

impl Add for Timings {
    type Output = Timings;

    fn add(mut self, rhs: Timings) -> Timings {
        self += rhs;
        self
    }
}

impl Timings {
    pub fn get(&self, metric: Timing) -> Duration {
        match metric {
            Timing::CompileTime => self.total,
            Timing::FrontendTime => self.total - self.certainly_backend,
            Timing::BackendTime => self.total - self.certainly_frontend,
        }
    }

    pub fn fuzz(&self) -> Duration {
        self.total - self.certainly_frontend - self.certainly_backend
    }

    fn from_json(timing: &json::UnitTime) -> Timings {
        let total = Duration::from_secs_f64(timing.duration);
        match timing.rmeta_time {
            Some(t) => Timings {
                total,
                certainly_frontend: Duration::from_secs_f64(t),
                certainly_backend: Duration::from_secs_f64(timing.duration - t),
            },
            None => Timings {
                total,
                certainly_frontend: Duration::ZERO,
                certainly_backend: Duration::ZERO,
            },
        }
    }

    fn parse_from_json(json: &[u8]) -> Result<Option<(json::Target, Timings)>, JobError> {
        if json.is_empty() {
            return Ok(None);
        }

        let json = facet_json::from_slice(json)?;
        let timings = Timings::from_json(&json);
        Ok(Some((json.target, timings)))
    }
}

impl Sum for Timings {
    fn sum<I: Iterator<Item = Timings>>(iter: I) -> Timings {
        iter.fold(Timings::default(), Add::add)
    }
}
