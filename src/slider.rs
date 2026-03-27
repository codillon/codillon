use crate::{
    dom_struct::DomStruct,
    dom_text::DomText,
    dom_vec::DomVec,
    jet::{AccessToken, Component, ElementFactory, ElementHandle, WithElement},
};
use num_format::{Locale, ToFormattedString};
use web_sys::{
    HtmlDataListElement, HtmlDivElement, HtmlInputElement, HtmlOptionElement, HtmlSpanElement,
};

type TickSpan = DomStruct<(DomText, ()), HtmlSpanElement>;

pub struct Slider {
    container: ElementHandle<HtmlDivElement>,
    ticks: DomVec<ElementHandle<HtmlOptionElement>, HtmlDataListElement>,
    tick_labels: DomVec<TickSpan, HtmlDivElement>,
    input: ElementHandle<HtmlInputElement>,
    factory: ElementFactory,
}

impl Slider {
    pub fn new(factory: ElementFactory) -> Self {
        let mut container = factory.div();
        let mut ticks = DomVec::new(factory.datalist());
        let mut tick_labels = DomVec::new(factory.div());
        let mut input = factory.input();
        input.set_attribute("type", "range");
        input.set_attribute("min", "0");
        input.set_attribute("class", "step-slider");
        input.set_attribute("list", "slider-ticks");
        ticks.set_attribute("id", "slider-ticks");
        container.set_attribute("class", "slider-container");
        tick_labels.set_attribute("class", "slider-ticks");
        container.append_node(&input);
        container.append_node(&ticks);
        container.append_node(&tick_labels);
        Self {
            container,
            ticks,
            tick_labels,
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
        self.tick_labels.remove_range(0, self.tick_labels.len());
        let mut add_tick = |step: usize, pos: f64| {
            let mut span = self.factory.span();
            span.set_attribute("class", "slider-tick");
            span.set_attribute("style", &format!("left:calc({pos:.4}%)"));
            self.tick_labels.push(DomStruct::new(
                (DomText::new(&step.to_formatted_string(&Locale::en)), ()),
                span,
            ));
            let mut option = self.factory.option();
            option.set_attribute("value", &step.to_string());
            self.ticks.push(option);
        };
        // Always include step 0 and last_step
        add_tick(0, 0.0);
        add_tick(last_step, 100.0);
        let interval = Self::tick_interval(last_step);
        let mut step = interval;
        while step < last_step - interval {
            add_tick(step, step as f64 / last_step as f64 * 100.0);
            step += interval;
        }
        // Only include the last interval tick if it is half an interval away from last step
        // to avoid crowding the last interval tick with last step tick
        let remainder = last_step % interval;
        if remainder == 0 || remainder >= interval / 2 {
            add_tick(step, step as f64 / last_step as f64 * 100.0);
        }
    }
}

impl Component for Slider {
    fn audit(&self) {
        self.container.audit();
        self.input.audit();
        self.ticks.audit();
    }
}

impl WithElement for Slider {
    type Element = HtmlDivElement;
    fn with_element(&self, f: impl FnMut(&HtmlDivElement), g: AccessToken) {
        self.container.with_element(f, g)
    }
}
