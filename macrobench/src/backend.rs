use smol::channel;

use crate::db;

#[derive(Debug)]
pub enum Message {
    Db(db::DbMessage),
    Notif(String),
}

impl From<db::DbMessage> for Message {
    fn from(value: db::DbMessage) -> Self {
        Message::Db(value)
    }
}

/// The aggregator for benchmarks and their results.
///
/// Notably, this struct combines an internal [database](db::Db)
/// with the threading machinery to receive benchmarking notifications.
pub struct Backend {
    messages: channel::Receiver<Message>,
    notifs: Vec<String>,
    db: db::Db,
    done: bool,
}

impl Backend {
    pub(crate) fn new(name: String, messages: channel::Receiver<Message>) -> Backend {
        Backend {
            messages,
            db: db::Db::new(name),
            notifs: Vec::new(),
            done: false,
        }
    }

    pub async fn update(&mut self) {
        match self.messages.recv().await {
            Ok(msg) => self.process(msg),
            Err(channel::RecvError) => {
                self.done = true;
            }
        };

        self.process_pending();
    }

    pub fn process_pending(&mut self) {
        loop {
            match self.messages.try_recv() {
                Ok(msg) => {
                    self.process(msg);
                    continue;
                }
                Err(channel::TryRecvError::Empty) => {}
                Err(channel::TryRecvError::Closed) => {
                    self.done = true;
                }
            }

            break;
        }
    }

    pub fn process(&mut self, msg: Message) {
        match msg {
            Message::Db(msg) => self.db.process(msg),
            Message::Notif(s) => self.notifs.push(s),
        }
    }

    pub async fn until_done(&mut self) {
        // make sure we don't miss the `Done`.
        self.process_pending();

        if !self.done {
            println!("frontend hung up while benchmarks still running..");
        }

        while !self.done {
            self.update().await;
        }
    }

    /// Read-only access to the underlying [results database][crate::db].
    pub fn db(&self) -> &db::Db {
        &self.db
    }

    pub fn notifs(&mut self) -> impl Iterator<Item = String> {
        self.notifs.drain(..)
    }
}
