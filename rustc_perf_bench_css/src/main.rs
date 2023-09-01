//! This benchmark tests the performance of CSS stylesheet parsing.
//! It uses the `lightningcss` crate, which internally uses `cssparser`, which is a browser-grade
//! CSS parser from Servo.
use lightningcss::stylesheet::{ParserOptions, StyleSheet};

static FB_CSS: &str = include_str!("../data/fb.css");

fn main() {
    // Inflate the CSS data a bit
    let fb_css_minified = FB_CSS.repeat(10);

    std::hint::black_box(StyleSheet::parse(&fb_css_minified, ParserOptions::default()).unwrap());
}
