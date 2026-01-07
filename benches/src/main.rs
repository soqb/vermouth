use macrobench::rustc::*;
use macrobench::*;

fn main() {
    macrobench::main("vermouth", |cx| {
        much_nesting(cx);
        many_tokens(cx);
        many_literals(cx);
    })
}

fn bench_job(
    group: &BenchGroup,
    target: &Target,
    name: &str,
    dir: &str,
    time: &str,
    i: u64,
    lvl: &str,
) {
    let mut cfg = JobCfg::default();
    cfg.rustflags = Some(format!("-Copt-level={lvl}").into());

    let bench = group.bench(format!("{name}-o{lvl}"), Time);
    let dir = format!("../target/bench/{dir}");

    MarkEnv { n: 4 }.measure(
        || target.new_job(&cfg, &dir, i).run(),
        || bench.warm(),
        |mk, mt| {
            let mt = mt.such_that(|tt| tt.name == time).next().unwrap();
            bench.mark(mk, mt.get(Timing::FrontendTime));
        },
    );
}

const OLVL: &[&str] = &["0", "1", "2", "3" /*, "s", "z"*/];

fn many_tokens(cx: &Bencher) {
    let mut target =
        Target::from_project("compile-fodder/many-tokens/Cargo.toml", Scenario::Fulldeps);
    target.rustflags = vec!["-Copt-level=0".into()];
    target.default_features = false;

    let target_vermouth = {
        let mut target = target.clone();
        target.features = vec!["vermouth".into()];
        target
    };

    let target_dtolnay = {
        let mut target = target.clone();
        target.features = vec!["dtolnay".into()];
        target
    };

    target.setup().unwrap();

    let frontend_time = cx.group("many-tokens");

    let bench_job = |target: &Target, name: &str, i: u64, lvl: &str| {
        bench_job(
            &frontend_time,
            target,
            name,
            "many-tokens",
            "many_tokens",
            i,
            lvl,
        )
    };

    bench_job(&target, "control", 0, "3");

    let run_for_all_opts = |target: &Target, name: &str| {
        for (i, lvl) in OLVL.iter().enumerate() {
            bench_job(target, name, i as u64, lvl)
        }
    };

    run_for_all_opts(&target_vermouth, "vermouth");
    run_for_all_opts(&target_dtolnay, "dtolnay");
}

fn many_literals(cx: &Bencher) {
    let mut target = Target::from_project(
        "compile-fodder/many-literals/Cargo.toml",
        Scenario::Fulldeps,
    );
    target.rustflags = vec!["-Copt-level=0".into()];
    target.default_features = false;

    let target_vermouth = {
        let mut target = target.clone();
        target.features = vec!["vermouth".into()];
        target
    };

    let target_dtolnay = {
        let mut target = target.clone();
        target.features = vec!["dtolnay".into()];
        target
    };

    target.setup().unwrap();

    let frontend_time = cx.group("many-literals");

    let bench_job = |target: &Target, name: &str, i: u64, lvl: &str| {
        bench_job(
            &frontend_time,
            target,
            name,
            "many-literals",
            "many_literals",
            i,
            lvl,
        )
    };

    bench_job(&target, "control", 0, "3");

    let run_for_all_opts = |target: &Target, name: &str| {
        for (i, lvl) in OLVL.iter().enumerate() {
            bench_job(target, name, i as u64, lvl)
        }
    };

    run_for_all_opts(&target_vermouth, "vermouth");
    run_for_all_opts(&target_dtolnay, "dtolnay");
}

fn much_nesting(cx: &Bencher) {
    let mut target =
        Target::from_project("compile-fodder/much-nesting/Cargo.toml", Scenario::Fulldeps);
    target.rustflags = vec!["-Copt-level=0".into()];
    target.default_features = false;

    let target_vermouth = {
        let mut target = target.clone();
        target.features = vec!["vermouth".into()];
        target
    };

    let target_dtolnay = {
        let mut target = target.clone();
        target.features = vec!["dtolnay".into()];
        target
    };

    target.setup().unwrap();

    let frontend_time = cx.group("much-nesting");

    let bench_job = |target: &Target, name: &str, i: u64, lvl: &str| {
        bench_job(
            &frontend_time,
            target,
            name,
            "much-nesting",
            "much_nesting",
            i,
            lvl,
        )
    };

    let run_for_all_opts = |target: &Target, name: &str| {
        for (i, lvl) in OLVL.iter().enumerate() {
            bench_job(target, name, i as u64, lvl)
        }
    };

    run_for_all_opts(&target_vermouth, "vermouth");
    run_for_all_opts(&target_dtolnay, "dtolnay");
}
