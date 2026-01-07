use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{MetricErased, stats};

/// The database of [benches](crate::Bench), [groups](crate::BenchGroup), and [results](stats::Wip)
/// used by the [frontend](crate::frontend::Frontend).
///
/// DB access is provided by [`Backend::db`](crate::backend::Backend::db)
/// and all public-facing methods on the database take `&self`.
pub struct Db {
    root_name: String,
    trunks: Vec<GroupAddr>,
    benches: Vec<DbBench>,
    groups: Vec<DbGroup>,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum Temperature {
    #[default]
    Cold,
    Warm,
    Hot,
    Frozen,
}

impl Temperature {
    pub fn set(&mut self, other: Temperature) {
        assert!(other >= *self);
        *self = other;
    }
}

#[derive(Debug)]
struct DbBench {
    inner: Bench,
    stats: stats::Wip,
    temp: Temperature,
}

impl From<Bench> for DbBench {
    fn from(inner: Bench) -> Self {
        DbBench {
            inner,
            stats: stats::Wip::default(),
            temp: Temperature::Cold,
        }
    }
}

#[derive(Debug, Clone)]
struct DbGroup {
    inner: Group,
    branches: Vec<GroupAddr>,
    benches: Vec<BenchAddr>,
}

impl From<Group> for DbGroup {
    fn from(inner: Group) -> Self {
        DbGroup {
            inner,
            branches: Vec::new(),
            benches: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GroupAddr(usize);

impl GroupAddr {
    // pub fn new(n: usize) -> GroupAddr {
    //     GroupAddr(n.checked_add(1).unwrap())
    // }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BenchAddr(usize);

#[derive(Debug)]
pub struct Bench {
    pub name: String,
    pub parent: GroupAddr,
    pub metric: MetricErased,
}

#[derive(Debug, Clone)]
pub struct Group {
    pub name: String,
    pub parent: Option<GroupAddr>,
}

#[derive(Debug)]
pub enum DbMessage {
    AllocGroup { addr: GroupAddr, group: Group },
    AllocBench { addr: BenchAddr, bench: Bench },
    PublishSample { addr: BenchAddr, sample: Vec<f64> },
    FreezeBench(BenchAddr),
    WarmupBench(BenchAddr),
}

// FIXME: messy but not sure how to approach.
pub trait DbSender {
    fn send(&self, m: DbMessage);
}

pub struct DbSocket<S> {
    tx: S,
    num_groups: AtomicUsize,
    num_benches: AtomicUsize,
}

impl<S: DbSender> DbSocket<S> {
    pub fn new(tx: S) -> DbSocket<S> {
        DbSocket {
            tx,
            num_groups: 0.into(),
            num_benches: 0.into(),
        }
    }

    pub fn tx(&self) -> &S {
        &self.tx
    }

    pub fn alloc_group(&self, group: Group) -> GroupAddr {
        let n = self.num_groups.fetch_add(1, Ordering::AcqRel);
        let addr = GroupAddr(n);
        self.tx.send(DbMessage::AllocGroup { addr, group });
        addr
    }

    pub fn alloc_bench(&self, bench: Bench) -> BenchAddr {
        let n = self.num_benches.fetch_add(1, Ordering::AcqRel);
        let addr = BenchAddr(n);
        self.tx.send(DbMessage::AllocBench { addr, bench });
        addr
    }

    pub fn publish_sample(&self, bench: BenchAddr, sample: Vec<f64>) {
        self.tx.send(DbMessage::PublishSample {
            addr: bench,
            sample,
        });
    }

    pub fn freeze_bench(&self, bench: BenchAddr) {
        self.tx.send(DbMessage::FreezeBench(bench));
    }

    pub fn warmup_bench(&self, bench: BenchAddr) {
        self.tx.send(DbMessage::WarmupBench(bench));
    }
}

// FIXME: this whole api is terrible. do better.
impl Db {
    /// An empty database.
    pub fn new(name: String) -> Db {
        Db {
            benches: Vec::new(),
            groups: Vec::new(),
            trunks: Vec::new(),
            root_name: name,
        }
    }

    pub fn process(&mut self, msg: DbMessage) {
        match msg {
            DbMessage::AllocGroup { addr, group } => {
                assert_eq!(addr.0, self.groups.len());
                let parent = group.parent;
                self.groups.push(group.into());
                match parent {
                    Some(parent) => self.groups[parent.0].branches.push(addr),
                    None => self.trunks.push(addr),
                }
            }
            DbMessage::AllocBench { addr, bench } => {
                assert_eq!(addr.0, self.benches.len());
                let group = bench.parent;
                self.benches.push(bench.into());
                self.groups[group.0].benches.push(addr);
            }
            DbMessage::FreezeBench(addr) => self.benches[addr.0].temp.set(Temperature::Frozen),
            DbMessage::WarmupBench(addr) => self.benches[addr.0].temp.set(Temperature::Warm),
            DbMessage::PublishSample { addr, sample } => {
                let bench = &mut self.benches[addr.0];
                bench.stats.update(&sample);
                bench.temp.set(Temperature::Hot);
            }
        }
    }

    pub fn name_of_group(&self, addr: Option<GroupAddr>) -> &str {
        match addr {
            Some(addr) => &self.groups[addr.0].inner.name,
            None => &self.root_name,
        }
    }

    pub fn benches_in(&self, addr: GroupAddr) -> impl ExactSizeIterator<Item = BenchAddr> {
        self.groups[addr.0].benches.iter().copied()
    }

    pub fn subgroups_of(
        &self,
        addr: Option<GroupAddr>,
    ) -> impl ExactSizeIterator<Item = GroupAddr> {
        let groups = match addr {
            Some(addr) => &self.groups[addr.0].branches,
            None => &self.trunks,
        };
        groups.iter().copied()
    }

    pub fn name_of_bench(&self, addr: BenchAddr) -> &str {
        &self.benches[addr.0].inner.name
    }

    pub fn snapshot_of_bench(&self, addr: BenchAddr) -> stats::Snapshot {
        self.benches[addr.0].stats.to_snapshot()
    }

    pub fn temperature_of_bench(&self, addr: BenchAddr) -> Temperature {
        self.benches[addr.0].temp
    }

    pub fn metric_for_bench(&self, addr: BenchAddr) -> &MetricErased {
        &self.benches[addr.0].inner.metric
    }

    // pub fn alloc_group(&self, group: Group) -> GroupAddr {
    //     let addr = self.groups.push_with(|addr| DbGroup {
    //         inner: group,
    //         addr: GroupAddr::new(addr),
    //     });
    //     GroupAddr::new(addr)
    // }

    // pub fn intern_bench(&self, bench: Bench) {
    //     self.benches.push(bench);
    // }

    // /// Builds a walkable tree from the contents of the internal buffers.
    // pub fn into_tree(self) -> GroupTree {
    //     let mut bench_map: HashMap<GroupAddr, Vec<Bench>> = HashMap::new();
    //     for bench in self.benches {
    //         bench_map.entry(bench.parent).or_default().push(bench);
    //     }

    //     let mut branch_map: HashMap<GroupAddr, Vec<(GroupAddr, String)>> = HashMap::new();
    //     for group in self.groups {
    //         branch_map
    //             .entry(group.inner.parent)
    //             .or_default()
    //             .push((group.addr, group.inner.name));
    //     }

    //     struct Frame {
    //         queue: std::vec::IntoIter<(GroupAddr, String)>,
    //         group: GroupTree,
    //     }

    //     let mut stack: Vec<Frame> = vec![];

    //     let mut new_frame = |addr, name| {
    //         let queue = branch_map.remove(&addr).map(IntoIterator::into_iter);

    //         let group = GroupTree {
    //             name,
    //             benches: bench_map.remove(&addr).unwrap_or_default(),
    //             branches: Vec::new(),
    //         };

    //         (queue, group)
    //     };

    //     let mut top = {
    //         let (queue, group) = new_frame(GroupAddr::ROOT, self.root_name);
    //         match queue {
    //             Some(queue) => Frame { queue, group },
    //             None => return group,
    //         }
    //     };
    //     loop {
    //         let Some((addr, name)) = top.queue.next() else {
    //             let group = top.group;
    //             if let Some(last) = stack.pop() {
    //                 top = last;
    //                 top.group.branches.push(group);
    //                 continue;
    //             } else {
    //                 return group;
    //             }
    //         };

    //         let (queue, group) = new_frame(addr, name);
    //         match queue {
    //             Some(queue) => {
    //                 stack.push(top);
    //                 top = Frame { queue, group }
    //             }
    //             None => top.group.branches.push(group),
    //         }
    //     }
    // }
}
