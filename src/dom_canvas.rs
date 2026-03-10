use crate::jet::{AccessToken, Component, ElementHandle, RenderWithToken, WithElement};
use wasm_bindgen::JsCast;
use web_sys::HtmlCanvasElement;

const WIDTH: f64 = 500.0;
const HEIGHT: f64 = 500.0;

pub enum Action {
    Extent(f64, f64, f64, f64),
    Color(i32, i32, i32),
    Radius(f64),
    Point(f64, f64),
    Clear,
}

pub struct DomCanvas {
    elem: ElementHandle<HtmlCanvasElement>,
    extent: (f64, f64, f64, f64),
    point_radius: f64,
}

impl DomCanvas {
    pub fn new(mut canvas: ElementHandle<HtmlCanvasElement>) -> Self {
        canvas.set_attribute("class", "graph-canvas");
        canvas.set_attribute("width", &WIDTH.to_string());
        canvas.set_attribute("height", &HEIGHT.to_string());
        Self {
            elem: canvas,
            extent: (-1.0, 1.0, -1.0, 1.0),
            point_radius: 3.0,
        }
    }

    pub fn with_2d_context<R>(
        &self,
        mut function: impl FnMut(&web_sys::CanvasRenderingContext2d) -> R,
        token: AccessToken,
    ) {
        self.elem.with_element(
            |canvas| {
                if let Ok(Some(contex_val)) = canvas.get_context("2d")
                    && let Ok(context) = contex_val.dyn_into::<web_sys::CanvasRenderingContext2d>()
                {
                    function(&context);
                }
            },
            token,
        );
    }

    pub fn render(&mut self, token: AccessToken, actions: &[Action]) {
        let tau = std::f64::consts::PI * 2.0;
        let mut extent = self.extent;
        let mut point_radius = self.point_radius;
        self.with_2d_context(
            |context| {
                use Action::*;
                for action in actions {
                    match *action {
                        Clear => context.clear_rect(0.0, 0.0, WIDTH, HEIGHT),
                        Point(x, y) => {
                            let (xmin, xmax, ymin, ymax) = extent;
                            let pixel_x = (x - xmin) / (xmax - xmin) * WIDTH;
                            let pixel_y = (y - ymin) / (ymax - ymin) * HEIGHT;
                            context.begin_path();
                            let _ = context.arc(pixel_x, pixel_y, point_radius, 0.0, tau);
                            context.fill();
                        }
                        Color(r, g, b) => {
                            context.set_fill_style_str(&format!("rgb({r}, {g}, {b})"))
                        }
                        Extent(xmin, xmax, ymin, ymax) => {
                            extent = (xmin, xmax, ymin, ymax);
                        }
                        Radius(radius) => point_radius = radius,
                    }
                }
            },
            token,
        );
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

impl RenderWithToken for DomCanvas {
    fn render(&mut self, token: AccessToken, actions: &[crate::dom_canvas::Action]) {
        self.render(token, actions)
    }
}
