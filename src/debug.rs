// Debug manager for time-travel instrumentation
// Currently support:
// - all normal scalar value ops: i32, i64, f32, f64 (pushes from instructions are recorded)
// - local/global set ops: i32, i64, f32, f64
// Currently do not support:
// - memory store ops: i32, i64, f32, f64 (stores record addr + value)
// - other memory operations
// - restoration of old values when a frame exits from a recursive call, or erasure when a frame repeats
// - all SIMD/vector operations
// - all reference types operations

use crate::{
    dom_canvas::{Action, DomCanvas},
    utils::FmtError,
};
use anyhow::{Context, Result, bail};
use js_sys::{Object, Reflect, WebAssembly::RuntimeError};
use regex::Regex;
use std::cell::Cell;
use thousands::{Separable, SeparatorPolicy};
use wasm_bindgen::{JsCast, prelude::*};

const MAX_STEP_COUNT: usize = 100_000;

// Debug state stored in Wasm memory
thread_local! {
    static TERMINATION: Cell<TerminationType> = Cell::new(Default::default());
    static ERROR_STR: CodillonCell<String> = CodillonCell::new(Default::default());
    static STEP_COUNT: Cell<usize> = const { Cell::new(0) };
    static STEPS: Box<[CodillonCell<ExecutionStep>]> = (0..MAX_STEP_COUNT)
                .map(|_| CodillonCell::new(ExecutionStep::default()))
                .collect();
    static INSTRUMENTATION_IMPORTS: Object = make_imports().expect("instrumentation");
}

// When a user Wasm module gets close to stack exhaustion, the browser can trap at any point
// in our code (without unwinding the stack), leaving a RefCell borrow flag set.
// Instead, we use our own RefCell approximation (still in safe Rust) that merely logs the issue.
// The worst-case consequence of a trap in an inconvenient place would be that the current (in-progress)
// step gets blanked to the default value.
// This is all pretty gross, but it seems difficult to gracefully handle a trap in the middle of
// "our" code (as called by the Wasm module).
struct CodillonCell<T: Default> {
    inner: Cell<T>,
    #[cfg(debug_assertions)]
    borrowed: Cell<bool>,
}

impl<T: Default> CodillonCell<T> {
    fn new(inner: T) -> Self {
        Self {
            inner: Cell::new(inner),
            #[cfg(debug_assertions)]
            borrowed: Cell::new(false),
        }
    }

    fn set(&self, inner: T) {
        self.inner.set(inner)
    }

    fn borrow(&self) {
        #[cfg(debug_assertions)]
        {
            if self.borrowed.get() {
                web_sys::console::log_1(&"warning: CodillonCell multiply borrowed".into());
            }
            self.borrowed.set(true);
        }
    }

    fn unborrow(&self) {
        #[cfg(debug_assertions)]
        self.borrowed.set(false);
    }

    fn with<U, F>(&self, func: F) -> U
    where
        F: FnOnce(&T) -> U,
    {
        self.borrow();
        let cur = self.inner.take();
        let ret = func(&cur);
        self.set(cur);
        self.unborrow();
        ret
    }

    fn with_mut<U, F>(&self, func: F) -> U
    where
        F: FnOnce(&mut T) -> U,
    {
        self.borrow();
        let mut cur = self.inner.take();
        let ret = func(&mut cur);
        self.set(cur);
        self.unborrow();
        ret
    }
}

#[derive(Clone, Copy, Debug)]
pub enum WasmValue {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl From<i32> for WasmValue {
    fn from(v: i32) -> Self {
        WasmValue::I32(v)
    }
}
impl From<i64> for WasmValue {
    fn from(v: i64) -> Self {
        WasmValue::I64(v)
    }
}
impl From<f32> for WasmValue {
    fn from(v: f32) -> Self {
        WasmValue::F32(v)
    }
}
impl From<f64> for WasmValue {
    fn from(v: f64) -> Self {
        WasmValue::F64(v)
    }
}

impl Separable for WasmValue {
    fn separate_by_policy(&self, policy: SeparatorPolicy) -> String {
        match self {
            WasmValue::I32(x) => x.separate_by_policy(policy),
            WasmValue::I64(x) => x.separate_by_policy(policy),
            WasmValue::F32(x) => format!("{:.3}", x)
                .trim_end_matches('0')
                .trim_end_matches('.')
                .to_string(),
            WasmValue::F64(x) => format!("{:.3}", x)
                .trim_end_matches('0')
                .trim_end_matches('.')
                .to_string(),
        }
    }
}

#[derive(Debug, Copy, Clone, Default, PartialEq)]
pub enum TerminationType {
    #[default]
    Running,
    TooManySteps,
    HitInvalid,
    HitBadImport,
    Error,
    Success,
}

fn reset_debug_state() {
    TERMINATION.set(Default::default());
    ERROR_STR.with(|e| e.set(String::new()));
    STEP_COUNT.set(0);
    with_current_step_mut(|step| step.reset());
}

fn commit_step() {
    assert!(STEP_COUNT.get() < MAX_STEP_COUNT);
    STEP_COUNT.set(STEP_COUNT.get() + 1);
    with_current_step_mut(|step| step.reset());
}

pub fn termination_type() -> TerminationType {
    TERMINATION.get()
}

pub fn step_count() -> usize {
    STEP_COUNT.get()
}

fn with_completed_step<T, F>(idx: usize, func: F) -> T
where
    F: FnOnce(&ExecutionStep) -> T,
{
    assert!(idx < MAX_STEP_COUNT);
    assert!(idx < step_count());
    STEPS.with(|step_cell| step_cell[idx].with(|step| func(step)))
}

fn with_current_step_mut<T, F>(func: F) -> T
where
    F: FnOnce(&mut ExecutionStep) -> T,
{
    STEPS.with(|step_cell| step_cell[step_count()].with_mut(|step| func(step)))
}

fn error_string() -> Option<String> {
    if termination_type() == TerminationType::Error {
        ERROR_STR.with(|e| e.with(|err| Some(err.clone())))
    } else {
        None
    }
}

#[derive(Default, Debug)]
struct ExecutionStep {
    line_num: u32,
    slot_assignments: Vec<(u32, WasmValue)>, // slot idx, value
    graphics_ops: Vec<Action>,               // XXX todo: memory
                                             // XXX todo: clear any func/frame on entry
                                             // XXX todo: save any func on call/call_indirect, restore after
}

impl ExecutionStep {
    fn reset(&mut self) {
        self.line_num = 0;
        self.slot_assignments.clear();
        self.graphics_ops.clear();
    }
}

#[derive(Default, Debug)]
pub struct ExecutionState {
    pub step: usize,
    pub slots: Vec<Option<WasmValue>>,
    pub status: Option<(usize, TerminationType, Option<String>)>,
}

impl ExecutionState {
    pub fn reset(&mut self, slot_count: usize) {
        self.step = 0;
        self.status = None;
        self.slots.resize(slot_count, None);
        for slot in self.slots.iter_mut() {
            *slot = None;
        }
    }

    pub fn goto_step(&mut self, target_step: usize, canvas: &mut DomCanvas) {
        if self.step > target_step || self.step == 0 {
            // XXX save checkpoints?
            self.reset(self.slots.len());
            canvas.reset();
            self.step = 0;
        }
        if step_count() == 0 {
            return;
        }
        assert!(target_step < step_count());
        for step in self.step..=target_step {
            with_completed_step(step, |s| {
                for (slot_idx, value) in &s.slot_assignments {
                    self.slots[*slot_idx as usize] = Some(*value);
                }
                canvas.render(&s.graphics_ops);
            });
        }
        self.step = target_step;
        self.status = Some((
            with_completed_step(target_step, |s| s.line_num as usize),
            if target_step + 1 == step_count() {
                termination_type()
            } else {
                TerminationType::Running
            },
            error_string(),
        ));
    }
}

fn register_closure<F>(obj: &Object, name: &str, func: Closure<F>)
where
    F: ?Sized + 'static + wasm_bindgen::closure::WasmClosure,
{
    Reflect::set(obj, &JsValue::from_str(name), func.as_ref().unchecked_ref()).ok();
    func.forget();
}

// Constructs the instrumentation functions for import
fn make_imports() -> Result<Object, JsValue> {
    let imports = Object::new();
    let debug_numbers = Object::new();

    // Updating the debug state at every step. Returns whether execution should continue.
    let step_closure = Closure::wrap(Box::new(move |line_num: u32| -> bool {
        use TerminationType::*;
        if termination_type() == HitBadImport && step_count() > 0 {
            with_completed_step(step_count() - 1, |completed| {
                with_current_step_mut(|current| {
                    current.line_num = completed.line_num;
                })
            });
        } else {
            with_current_step_mut(|step| step.line_num = line_num);
        }

        commit_step();

        match termination_type() {
            Running => (),
            TooManySteps => panic!("execution unexpectedly continued after TooManySteps"),
            HitInvalid | HitBadImport => return false,
            Error => panic!("execution unexpectedly continued after Error"),
            Success => panic!("execution unexpectedly continued after Success"),
        }

        // Signal halt
        if step_count() >= MAX_STEP_COUNT - 1 {
            TERMINATION.set(TerminationType::TooManySteps);
            return false;
        }

        true
    }) as Box<dyn Fn(u32) -> bool>);
    register_closure(&debug_numbers, "record_step", step_closure);

    let record_invalid =
        Closure::wrap(
            Box::new(move || TERMINATION.set(TerminationType::HitInvalid)) as Box<dyn Fn()>,
        );
    register_closure(&debug_numbers, "record_invalid", record_invalid);

    let func_placeholder =
        Closure::wrap(
            Box::new(move || TERMINATION.set(TerminationType::HitBadImport)) as Box<dyn Fn()>,
        );
    register_closure(&debug_numbers, "func_placeholder", func_placeholder);

    create_closure_record_operations(&debug_numbers);

    Reflect::set(&imports, &"codillon_debug".into(), &debug_numbers)?;

    {
        let draw = Object::new();
        create_graphics_helpers(&draw);
        Reflect::set(&imports, &"draw".into(), &draw)?;
    }

    Ok(imports)
}

fn create_graphics_helpers(draw: &Object) {
    let draw_point = Closure::wrap(Box::new(move |x: f64, y: f64| {
        with_current_step_mut(|step| step.graphics_ops.push(Action::Point(x, y)))
    }) as Box<dyn Fn(f64, f64)>);
    register_closure(draw, "point", draw_point);
    let clear_canvas = Closure::wrap(Box::new(move || {
        with_current_step_mut(|step| step.graphics_ops.push(Action::Clear))
    }) as Box<dyn Fn()>);
    register_closure(draw, "clear", clear_canvas);
    let set_color = Closure::wrap(Box::new(move |r: i32, g: i32, b: i32| {
        with_current_step_mut(|step| step.graphics_ops.push(Action::Color(r, g, b)))
    }) as Box<dyn Fn(i32, i32, i32)>);
    register_closure(draw, "set_color", set_color);
    let set_extent = Closure::wrap(Box::new(move |xmin: f64, xmax: f64, ymin: f64, ymax: f64| {
        with_current_step_mut(|step| {
            step.graphics_ops
                .push(Action::Extent(xmin, xmax, ymin, ymax))
        })
    }) as Box<dyn Fn(f64, f64, f64, f64)>);
    register_closure(draw, "set_extent", set_extent);
    let set_radius = Closure::wrap(Box::new(move |radius: f64| {
        with_current_step_mut(|step| step.graphics_ops.push(Action::Radius(radius)))
    }) as Box<dyn Fn(f64)>);
    register_closure(draw, "set_radius", set_radius);
}

fn create_closure_record_operations(obj: &Object) {
    let record = |value: WasmValue, slot: u32| {
        with_current_step_mut(|step| step.slot_assignments.push((slot, value)))
    };
    let record_i32 = Closure::wrap(
        Box::new(move |value: i32, slot: u32| record(value.into(), slot)) as Box<dyn Fn(i32, u32)>,
    );
    register_closure(obj, "record_i32", record_i32);

    let record_f32 = Closure::wrap(
        Box::new(move |value: f32, slot: u32| record(value.into(), slot)) as Box<dyn Fn(f32, u32)>,
    );
    register_closure(obj, "record_f32", record_f32);

    let record_i64 = Closure::wrap(
        Box::new(move |value: i64, slot: u32| record(value.into(), slot)) as Box<dyn Fn(i64, u32)>,
    );
    register_closure(obj, "record_i64", record_i64);

    let record_f64 = Closure::wrap(
        Box::new(move |value: f64, slot: u32| record(value.into(), slot)) as Box<dyn Fn(f64, u32)>,
    );
    register_closure(obj, "record_f64", record_f64);
}

pub async fn run_binary(binary: &[u8]) -> Result<()> {
    use js_sys::{Function, Reflect};
    reset_debug_state();
    let imports = INSTRUMENTATION_IMPORTS.with(|imports| imports.clone());
    let promise = js_sys::WebAssembly::instantiate_buffer(binary, &imports);
    let js_value = wasm_bindgen_futures::JsFuture::from(promise)
        .await
        .fmt_err()?;
    let instance = Reflect::get(&js_value, &JsValue::from_str("instance")).fmt_err()?;
    let exports = Reflect::get(&instance, &JsValue::from_str("exports")).fmt_err()?;
    // Call main function with default values for its params
    match Reflect::get(&exports, &JsValue::from_str("main")) {
        Ok(main) => {
            let main = wasm_bindgen::JsCast::dyn_ref::<Function>(&main)
                .context("main is not an exported function")?;
            match main.apply(&JsValue::null(), &js_sys::Array::new()) {
                Ok(val) if val.is_undefined() => {
                    match termination_type() {
                        TerminationType::Running => TERMINATION.set(TerminationType::Success),
                        TerminationType::TooManySteps
                        | TerminationType::HitInvalid
                        | TerminationType::HitBadImport
                        | TerminationType::Success
                        | TerminationType::Error => {
                            unreachable!("success after other result");
                        }
                    }
                    Ok(())
                }
                Ok(val) => {
                    bail!("unhandled return value from function: {:?}", val)
                }
                Err(e) => {
                    let reason: String = if let Some(r) = e.dyn_ref::<RuntimeError>() {
                        r.message().into()
                    } else {
                        let re = Regex::new(r"\((.*)")?;
                        let debug_string = format!("{e:?}");
                        if let Some(captures) = re.captures(&debug_string) {
                            captures[1].to_string()
                        } else {
                            "unknown error".to_string()
                        }
                    };
                    match termination_type() {
                        TerminationType::Running => {
                            if step_count() > 0 {
                                with_completed_step(step_count() - 1, |completed| {
                                    with_current_step_mut(|current| {
                                        current.line_num = completed.line_num;
                                    })
                                });
                            }
                            commit_step();
                            TERMINATION.set(TerminationType::Error);
                            ERROR_STR.with(|e| e.set(reason));
                        }
                        TerminationType::HitInvalid
                        | TerminationType::HitBadImport
                        | TerminationType::TooManySteps => {} // error is expected
                        TerminationType::Success | TerminationType::Error => {
                            unreachable!("error after other result");
                        }
                    }
                    Ok(())
                }
            }
        }
        Err(e) => bail!("reflection failure: {:?}", e),
    }
}
