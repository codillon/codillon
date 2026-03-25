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

use crate::utils::FmtError;
use anyhow::{Context, Result, bail};
use js_sys::{Object, Reflect, WebAssembly::RuntimeError};
use std::cell::{Ref, RefCell};
use wasm_bindgen::{JsCast, prelude::*};

const MAX_STEP_COUNT: usize = 1_000;

// Debug state stored in Wasm memory
thread_local! {
    static STATE: RefCell<DebugState> = RefCell::new(DebugState::default());
    static INSTRUMENTATION_IMPORTS: Object = make_imports().expect("instrumentation");
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

#[derive(Debug, Default)]
pub enum TerminationType {
    #[default]
    Running,
    TooManySteps,
    HitInvalid,
    HitBadImport,
    Error(String),
    Success,
}

#[derive(Default)]
pub struct DebugState {
    pub current_step: ExecutionStep,
    pub completed_steps: Vec<ExecutionStep>,
    pub termination: TerminationType,
}

impl DebugState {
    fn reset(&mut self) {
        self.current_step = Default::default();
        self.completed_steps.clear();
        self.termination = Default::default();
    }
}

#[derive(Default, Debug)]
pub struct ExecutionStep {
    pub line_num: u32,
    pub slot_assignments: Vec<(u32, WasmValue)>, // slot idx, value
                                                 // XXX todo: memory
                                                 // XXX todo: canvas operations
}

fn register_closure<F>(obj: &Object, name: &str, func: Closure<F>)
where
    F: ?Sized + 'static + wasm_bindgen::closure::WasmClosure,
{
    Reflect::set(obj, &JsValue::from_str(name), func.as_ref().unchecked_ref()).ok();
    func.forget();
}

pub fn reset_debug_state() {
    STATE.with(|cur_state| cur_state.borrow_mut().reset())
}

pub fn with_debug_state<T, F>(func: F) -> T
where
    F: FnOnce(Ref<'_, DebugState>) -> T,
{
    STATE.with(|state| func(state.borrow()))
}

// Constructs the instrumentation functions for import
fn make_imports() -> Result<Object, JsValue> {
    let imports = Object::new();
    let debug_numbers = Object::new();

    // Updating the debug state at every step. Returns whether execution should continue.
    let step_closure = Closure::wrap(Box::new(move |line_num: u32| -> bool {
        use TerminationType::*;
        STATE.with_borrow_mut(|state| {
            state.current_step.line_num = line_num;

            state
                .completed_steps
                .push(std::mem::take(&mut state.current_step));

            match state.termination {
                Running => (),
                TooManySteps => panic!("execution unexpectedly continued after TooManySteps"),
                HitInvalid | HitBadImport => return false,
                Error(_) => panic!("execution unexpectedly continued after Error"),
                Success => panic!("execution unexpectedly continued after Success"),
            }

            // Signal halt
            if state.completed_steps.len() > MAX_STEP_COUNT {
                state.termination = TerminationType::TooManySteps;
                return false;
            }

            true
        })
    }) as Box<dyn Fn(u32) -> bool>);
    register_closure(&debug_numbers, "record_step", step_closure);

    let record_invalid = Closure::wrap(Box::new(move || {
        STATE.with_borrow_mut(|state| state.termination = TerminationType::HitInvalid)
    }) as Box<dyn Fn()>);
    register_closure(&debug_numbers, "record_invalid", record_invalid);

    let func_placeholder = Closure::wrap(Box::new(move || {
        STATE.with_borrow_mut(|state| state.termination = TerminationType::HitBadImport)
    }) as Box<dyn Fn()>);
    register_closure(&debug_numbers, "func_placeholder", func_placeholder);

    create_closure_record_operations(&debug_numbers);

    Reflect::set(&imports, &"codillon_debug".into(), &debug_numbers)?;
    Ok(imports)
}

/* XXX: restore graphics features
fn create_closure_helpers(import: &Object) {
    let helpers = Object::new();
    let add_change = |change: SparseChange| {
        STATE.with(|cur_state| {
            let mut state = cur_state.borrow_mut();
            let step_idx = state.changes.len();
            state.sparse_changes.push((step_idx, change));
        });
    };
    let draw_point = Closure::wrap(Box::new(move |x: f64, y: f64| {
        add_change(SparseChange::Point(x, y));
    }) as Box<dyn Fn(f64, f64)>);
    register_closure(&helpers, "draw_point", draw_point);
    let clear_canvas = Closure::wrap(Box::new(move || {
        add_change(SparseChange::Clear());
    }) as Box<dyn Fn()>);
    register_closure(&helpers, "clear_canvas", clear_canvas);
    let set_color = Closure::wrap(Box::new(move |r: i32, g: i32, b: i32| {
        add_change(SparseChange::Color(r, g, b));
    }) as Box<dyn Fn(i32, i32, i32)>);
    register_closure(&helpers, "set_color", set_color);
    let set_extent = Closure::wrap(Box::new(move |xmin: f64, xmax: f64, ymin: f64, ymax: f64| {
        add_change(SparseChange::Extent(xmin, xmax, ymin, ymax));
    }) as Box<dyn Fn(f64, f64, f64, f64)>);
    register_closure(&helpers, "set_extent", set_extent);
    let set_radius = Closure::wrap(Box::new(move |radius: f64| {
        add_change(SparseChange::Radius(radius));
    }) as Box<dyn Fn(f64)>);
    register_closure(&helpers, "set_radius", set_radius);
    Reflect::set(import, &JsValue::from_str("helpers"), &helpers).ok();
}
*/

fn create_closure_record_operations(obj: &Object) {
    let record = |value: WasmValue, slot: u32| {
        STATE.with(|state| {
            state
                .borrow_mut()
                .current_step
                .slot_assignments
                .push((slot, value))
        });
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
                    STATE.with_borrow_mut(|state| match state.termination {
                        TerminationType::Running => state.termination = TerminationType::Success,
                        TerminationType::TooManySteps
                        | TerminationType::HitInvalid
                        | TerminationType::HitBadImport
                        | TerminationType::Success
                        | TerminationType::Error(..) => {
                            unreachable!("success after other result");
                        }
                    });
                    Ok(())
                }
                Ok(val) => {
                    bail!("unhandled return value from function: {:?}", val)
                }
                Err(e) => match e.dyn_ref::<RuntimeError>() {
                    Some(r) => {
                        let reason: String = r.message().into();
                        STATE.with_borrow_mut(|state| match state.termination {
                            TerminationType::Running => {
                                state.termination = TerminationType::Error(reason)
                            }
                            TerminationType::HitInvalid
                            | TerminationType::HitBadImport
                            | TerminationType::TooManySteps => {} // error is expected
                            TerminationType::Success | TerminationType::Error(..) => {
                                unreachable!("error after other result");
                            }
                        });
                        Ok(())
                    }
                    None => {
                        bail!("other failure: {:?}", e);
                    }
                },
            }
        }
        Err(e) => bail!("reflection failure: {:?}", e),
    }
}
