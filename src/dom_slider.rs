use crate::{
    dom_vec::DomVec,
    jet::{AccessToken, Component, ElementFactory, ElementHandle, WithElement},
};
use web_sys::{HtmlDataListElement, HtmlDivElement, HtmlInputElement, HtmlOptionElement};

pub struct DomSlider {
    container: ElementHandle<HtmlDivElement>,
    ticks: DomVec<ElementHandle<HtmlOptionElement>, HtmlDataListElement>,
    input: ElementHandle<HtmlInputElement>,
    factory: ElementFactory,
}

impl DomSlider {
    pub fn new(factory: ElementFactory) -> Self {
        let mut container = factory.div();
        let mut ticks = DomVec::new(factory.datalist());
        let mut input = factory.input();
        input.set_attribute("type", "range");
        input.set_attribute("min", "0");
        input.set_attribute("class", "step-slider");
        input.set_attribute("list", "slider-ticks");
        ticks.set_attribute("id", "slider-ticks");
        container.set_attribute("class", "slider-container");
        container.append_node(&input);
        container.append_node(&ticks);
        Self {
            container,
            ticks,
            input,
            factory,
        }
    }

    pub fn show(&mut self) {
        self.container.set_attribute("class", "slider-container")
    }

    pub fn hide(&mut self) {
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

    pub fn build_ticks(&mut self, last_step: usize) {
        assert!(last_step > 0); // logic error to use slider if there's only one step
        self.input.set_attribute("max", &last_step.to_string());
        self.ticks.remove_range(0, self.ticks.len());
        let tick = |step: usize, pos: f64| {
            let mut option = self.factory.option();
            option.set_attribute("value", &step.to_string());
            option.set_attribute("label", &step.to_string());
            let label_len = step.to_string().len() as f64;
            // Negative margin-right to cancel out own width
            option.set_attribute(
                "style",
                &format!("margin: 0 -{label_len}ch 0 calc({pos:.4}% - {pos:.0}px / 10)"),
            );
            option
        };
        // Always include step 0 and last_step
        self.ticks.push(tick(0, 0.0));
        let interval = Self::tick_interval(last_step);
        let mut step = interval;
        let mut prev_pos = 0.0_f64;
        while step < last_step - interval {
            let pos = step as f64 / last_step as f64 * 100.0;
            self.ticks.push(tick(step, pos - prev_pos));
            prev_pos = pos;
            step += interval;
        }
        // Only include the last interval tick if it is half an interval away from last step
        // to avoid crowding the last interval tick with last step tick
        let remainder = last_step % interval;
        if remainder == 0 || remainder >= interval / 2 {
            let pos = step as f64 / last_step as f64 * 100.0;
            self.ticks.push(tick(step, pos - prev_pos));
            prev_pos = pos;
        }
        self.ticks.push(tick(last_step, 100.0 - prev_pos));
    }
}

impl Component for DomSlider {
    fn audit(&self) {
        self.container.audit();
        self.input.audit();
        self.ticks.audit();
    }
}

impl WithElement for DomSlider {
    type Element = HtmlDivElement;
    fn with_element(&self, f: impl FnMut(&HtmlDivElement), g: AccessToken) {
        self.container.with_element(f, g)
    }
}
