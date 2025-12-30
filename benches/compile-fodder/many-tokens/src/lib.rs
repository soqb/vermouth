//! # Methodology
//!
//! We're testing the raw token-pumping performance of the compiled proc-macro, under various tunings.
//! To make accurate measurements of the overhead involved, we run control benchmarks which do not themselves quote.

// no, of course we don't need this.
#[unsafe(no_mangle)]
pub fn light_it_up() {
    upstream::feel_the_burn! {}
}
