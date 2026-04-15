use crate::jet::{AccessToken, Component, ElementHandle, WithElement};
use anyhow::Result;
use web_sys::{CanvasRenderingContext2d, HtmlCanvasElement};

const WIDTH: f64 = 256.0;
const HEIGHT: f64 = 256.0;

#[derive(Debug)]
pub enum Action {
    Extent(f64, f64, f64, f64),
    Color(i32, i32, i32),
    Radius(f64),
    Point(f64, f64),
    Clear,
}

pub struct DomCanvas {
    elem: ElementHandle<HtmlCanvasElement>,
    context: CanvasRenderingContext2d,
    extent: (f64, f64, f64, f64),
    point_radius: f64,
}

impl DomCanvas {
    pub fn new(mut canvas: ElementHandle<HtmlCanvasElement>) -> Result<Self> {
        canvas.set_attribute("class", "graph-canvas");
        canvas.set_attribute("width", &WIDTH.to_string());
        canvas.set_attribute("height", &HEIGHT.to_string());
        let mut ret = Self {
            context: canvas.get_context()?,
            elem: canvas,
            extent: (-1.0, 1.0, -1.0, 1.0),
            point_radius: 3.0,
        };
        ret.reset();
        Ok(ret)
    }

    pub fn reset(&mut self) {
        self.render(&[
            Action::Clear,
            Action::Color(0, 0, 0),
            Action::Radius(3.0),
            Action::Extent(-1.0, 1.0, -1.0, 1.0),
        ]);
    }

    pub fn render(&mut self, actions: &[Action]) {
        const TAU: f64 = std::f64::consts::PI * 2.0;
        let mut extent = self.extent;
        let mut point_radius = self.point_radius;
        use Action::*;
        for action in actions {
            match *action {
                Clear => self.context.clear_rect(0.0, 0.0, WIDTH, HEIGHT),
                Point(x, y) => {
                    let (xmin, xmax, ymin, ymax) = extent;
                    let pixel_x = (x - xmin) / (xmax - xmin) * WIDTH;
                    let pixel_y = ymax * HEIGHT - (y - ymin) / (ymax - ymin) * HEIGHT;
                    self.context.begin_path();
                    self.context
                        .arc(pixel_x, pixel_y, point_radius, 0.0, TAU)
                        .unwrap();
                    self.context.fill();
                }
                Color(r, g, b) => self
                    .context
                    .set_fill_style_str(&format!("rgb({r}, {g}, {b})")),
                Extent(xmin, xmax, ymin, ymax) => {
                    extent = (xmin, xmax, ymin, ymax);
                }
                Radius(radius) => point_radius = radius,
            }
        }

        self.extent = extent;
        self.point_radius = point_radius;
    }
}

impl Component for DomCanvas {
    fn audit(&self) {
        self.elem.audit();
    }
}

impl WithElement for DomCanvas {
    type Element = HtmlCanvasElement;
    fn with_element(&self, f: impl FnMut(&Self::Element), g: AccessToken) {
        self.elem.with_element(f, g);
    }
}
