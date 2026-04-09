use crate::{
    debug::TerminationType,
    dom_struct::DomStruct,
    dom_text::DomText,
    dom_vec::DomVec,
    jet::{AccessToken, Component, ElementFactory, ElementHandle, WithElement},
};
use thousands::Separable;
use web_sys::{HtmlDivElement, HtmlInputElement, HtmlSpanElement};

type TickSpan = DomStruct<(DomText, ()), HtmlSpanElement>;

pub struct Slider {
    container: ElementHandle<HtmlDivElement>,
    tick_labels: DomVec<TickSpan, HtmlDivElement>,
    input: ElementHandle<HtmlInputElement>,
    factory: ElementFactory,
    visible: bool,
}

impl Slider {
    pub fn new(factory: ElementFactory) -> Self {
        let mut container = factory.div();
        let mut tick_labels = DomVec::new(factory.div());
        let mut input = factory.input();
        input.set_attribute("type", "range");
        input.set_attribute("min", "0");
        input.set_attribute("class", "step-slider");
        input.set_attribute("list", "slider-ticks");
        container.set_attribute("class", "slider-container");
        tick_labels.set_attribute("class", "slider-ticks");
        container.append_node(&tick_labels);
        container.append_node(&input);
        Self {
            container,
            tick_labels,
            input,
            factory,
            visible: true,
        }
    }

    pub fn is_visible(&self) -> bool {
        self.visible
    }

    pub fn show(&mut self) {
        self.visible = true;
        self.container.set_attribute("class", "slider-container")
    }

    pub fn hide(&mut self) {
        self.visible = false;
        self.container
            .set_attribute("class", "slider-container slider-hidden")
    }

    pub fn value_as_number(&self) -> f64 {
        self.input.value_as_number()
    }

    pub fn set_value_as_number(&mut self, value: f64) {
        self.input.set_value_as_number(value)
    }

    fn tick_interval(last_step: usize) -> usize {
        if last_step <= 9 {
            return 1;
        }
        // Interval is always a power of 10
        let interval = 10_usize.pow(last_step.ilog10()) / 10;
        // What quarter of the magnitude the last step is at.
        let quarter = last_step / (interval * 25);
        // use 1x, 2x, or 5x multiples of 10 for intervals
        match quarter {
            0 => interval,
            1 => interval * 2,
            2 => interval * 5,
            _ => interval * 10,
        }
    }

    pub fn build_ticks(
        &mut self,
        last_step: usize,
        current_step: usize,
        termination: &TerminationType,
    ) {
        assert!(last_step > 0); // logic error to use slider if there's only one step
        self.input.set_attribute("max", &last_step.to_string());
        self.tick_labels.remove_range(0, self.tick_labels.len());
        let interval = Self::tick_interval(last_step);
        let remainder = last_step % interval;
        let mut step = 0;
        let mut add_tick = |step: usize, class: &str, substitute_str: Option<&str>| {
            let pos = step as f64 / last_step as f64 * 100.0;
            let mut span = self.factory.span();
            span.set_attribute("class", class);
            span.set_attribute("style", &format!("left:calc({pos:.4}%)"));
            self.tick_labels.push(DomStruct::new(
                (
                    DomText::new(substitute_str.unwrap_or(&step.separate_with_commas())),
                    (),
                ),
                span,
            ));
        };
        while step < last_step {
            // Don't include the last interval tick if it is less than half an interval away from last step
            // to avoid crowding the last interval tick with last step tick
            if step + interval > last_step && remainder != 0 && remainder <= interval / 2 {
                break;
            }
            if step != current_step {
                add_tick(step, "slider-tick", None);
            }
            step += interval;
        }
        let (class, substitute_str) = match termination {
            TerminationType::Success if last_step == current_step => {
                ("slider-tick current-slider-tick", None)
            }
            TerminationType::Success => ("slider-tick", None),
            TerminationType::TooManySteps => ("slider-tick bad-slider-tick", Some("\u{221e}")),
            _ => ("slider-tick bad-slider-tick", None),
        };

        add_tick(last_step, class, substitute_str);
        if current_step != last_step {
            add_tick(current_step, "slider-tick current-slider-tick", None);
        }
    }
}

impl Component for Slider {
    fn audit(&self) {
        self.container.audit();
        self.input.audit();
        self.tick_labels.audit();
    }
}

impl WithElement for Slider {
    type Element = HtmlDivElement;
    fn with_element(&self, f: impl FnMut(&HtmlDivElement), g: AccessToken) {
        self.container.with_element(f, g)
    }
}
