use std::time::{Duration, Instant};

use ratatui::{
    DefaultTerminal, Frame,
    layout::{
        Constraint::{Length, Max},
        Rect,
    },
    style::{Color, Style},
    text::{Line, Span},
    widgets::{Block, Cell, Padding, Row, Table, TableState},
};
use smol::stream::StreamExt;

use crate::{
    backend, db,
    frontend::{Frontend, Treamble},
};

pub struct Tui {}

impl Frontend for Tui {
    async fn lifecycle(self, backend: &mut backend::Backend) -> anyhow::Result<()> {
        let mut term = ratatui::init();
        let s = Runtime::from_backend(backend);

        let result = s.run(&mut term).await;

        ratatui::restore();
        result
    }
}

pub struct Runtime<'a> {
    backend: &'a mut backend::Backend,
    results_table_state: TableState,
    next_frame: Instant,
}

impl<'a> Runtime<'a> {
    fn from_backend(backend: &'a mut backend::Backend) -> Runtime<'a> {
        Runtime {
            backend,
            results_table_state: TableState::default(),
            next_frame: Instant::now(),
        }
    }

    fn db(&self) -> &db::Db {
        self.backend.db()
    }

    pub async fn run(mut self, term: &mut DefaultTerminal) -> anyhow::Result<()> {
        let mut redraw = true;
        let mut stream = crossterm::event::EventStream::new();

        loop {
            if redraw {
                redraw = false;
                term.draw(|frame| self.draw_to(frame)).unwrap();
            }

            enum Ev {
                Done,
                Input(crossterm::event::Event),
                DbNotes,
            }

            // updates from input are instant, but updates from backend wait at least 80ms.
            let ev = smol::future::or(
                async { Ok(stream.next().await.transpose()?.map_or(Ev::Done, Ev::Input)) },
                async {
                    smol::Timer::at(self.next_frame).await;
                    self.next_frame = Instant::now() + Duration::from_millis(80);
                    self.backend.update().await;
                    Ok(Ev::DbNotes)
                },
            )
            .await;
            let ev = match ev {
                Err(err) => return Err(err),
                Ok(Ev::Input(ev)) => ev,
                Ok(Ev::Done) => break,
                Ok(Ev::DbNotes) => {
                    redraw = true;
                    continue;
                }
            };

            use crossterm::event::*;
            match ev {
                Event::Key(KeyEvent { code, .. }) if code == KeyCode::Char('q') => break,
                Event::Resize(_, _) => redraw = true,
                Event::Mouse(MouseEvent {
                    kind: ev,
                    column,
                    row,
                    ..
                }) => match ev {
                    MouseEventKind::ScrollDown => self.results_table_state.scroll_down_by(1),
                    MouseEventKind::ScrollUp => self.results_table_state.scroll_up_by(1),
                    MouseEventKind::ScrollLeft => self.results_table_state.scroll_left_by(1),
                    MouseEventKind::ScrollRight => self.results_table_state.scroll_right_by(1),
                    _ => {}
                },
                _ => {}
            }
        }

        Ok(())
    }

    pub fn draw_to(&mut self, frame: &mut Frame) {
        let (root_area, status_area) = {
            let mut results_area = frame.area();

            if let Some(spare) = results_area.width.checked_sub(100) {
                results_area.x += spare / 2;
                results_area.width = results_area.width - spare;
            }

            results_area.height -= 3;

            let status_area = Rect {
                y: results_area.bottom(),
                height: 3,
                ..results_area
            };

            let border = Block::bordered()
                .padding(Padding::symmetric(3, 1))
                .title_top(format!("results ({})", self.db().name_of_group(None)));
            frame.render_widget(&border, results_area);
            let results_area = border.inner(results_area);

            let border = Block::bordered()
                .padding(Padding::symmetric(0, 0))
                .title_top("status");
            frame.render_widget(&border, status_area);
            let status_area = border.inner(status_area);

            (results_area, status_area)
        };

        let table = self.to_table_widget();
        frame.render_stateful_widget(&table, root_area, &mut self.results_table_state);

        let status = self.backend.notifs().last().unwrap_or_default();
        frame.render_widget(Line::from(status), status_area);
    }

    pub fn to_table_widget(&self) -> Table<'static> {
        let mut rows = Vec::new();
        for subgroup in self.db().subgroups_of(None) {
            rows.push(Row::new([""; 4]));
            self.to_rows(Treamble::default(), subgroup, &mut rows);
        }

        let bold = Style::new().bold();

        Table::default()
            .header(Row::new([
                Cell::new("benchmark").style(bold),
                Cell::new("n").style(bold),
                Cell::new("μ").style(bold),
                Cell::new("σ").style(bold),
            ]))
            .widths([Max(60), Length(10), Length(10), Length(10)])
            .rows(rows)
    }

    fn to_rows(&self, fmt: Treamble, group: db::GroupAddr, table: &mut Vec<Row<'static>>) {
        table.push(Row::new([
            format!("{fmt}{name}", name = self.db().name_of_group(Some(group))),
            String::new(),
            String::new(),
        ]));

        let benches = self.db().benches_in(group);
        let last_bench = benches.len().checked_sub(1);
        for (i, bench) in benches.enumerate() {
            let (hide, color) = match self.db().temperature_of_bench(bench) {
                db::Temperature::Cold => (true, Color::Reset),
                db::Temperature::Warm => (true, Color::Yellow),
                db::Temperature::Hot => (false, Color::Yellow),
                db::Temperature::Frozen => (false, Color::Green),
            };

            let titleline = Line::default().spans([
                fmt.nest(Some(i) == last_bench).to_string().into(),
                Span::styled(self.db().name_of_bench(bench).to_string(), color),
            ]);

            if hide {
                table.push(Row::new([titleline, "".into(), "".into(), "".into()]));
                continue;
            }

            let snap = self.db().snapshot_of_bench(bench);
            let metric = self.db().metric_for_bench(bench);
            table.push(Row::new([
                titleline,
                format!("{}: {}", metric.name(), snap.n).into(),
                metric.display(snap.mean).to_string().into(),
                format!("±{}", metric.display(snap.unbiased_sample_variance.sqrt())).into(),
            ]));
        }

        let subgroups = self.db().subgroups_of(Some(group));
        let last_subgroup = subgroups.len().checked_sub(1);
        for (i, subgroup) in subgroups.enumerate() {
            self.to_rows(fmt.nest(Some(i) == last_subgroup), subgroup, table);
        }
    }
}
