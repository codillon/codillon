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
    utils::{FmtError, SlotConnections},
};
use anyhow::{Context, Result, bail};
use arrayvec::ArrayVec;
use js_sys::{Object, Reflect, WebAssembly::RuntimeError};
use regex::Regex;
use smallvec::SmallVec;
use std::cell::Cell;
use thousands::{Separable, SeparatorPolicy};
use wasm_bindgen::{JsCast, prelude::*};

const MAX_STEP_COUNT: usize = 100_001;

pub struct RunLog {
    termination: Cell<TerminationType>,
    error_str: CodillonCell<String>,
    step_count: Cell<usize>,
    steps: Box<[CodillonCell<ExecutionStep>]>,
    has_graphics: Cell<bool>,
}

impl Default for RunLog {
    fn default() -> Self {
        Self {
            termination: Cell::new(Default::default()),
            error_str: CodillonCell::new(Default::default()),
            step_count: Cell::new(0),
            steps: (0..MAX_STEP_COUNT)
                .map(|_| CodillonCell::new(ExecutionStep::default()))
                .collect(),
            has_graphics: Cell::new(Default::default()),
        }
    }
}

impl RunLog {
    pub fn termination_type(&self) -> TerminationType {
        self.termination.get()
    }

    pub fn set_termination_type(&self, tt: TerminationType) {
        self.termination.set(tt)
    }

    pub fn step_count(&self) -> usize {
        self.step_count.get()
    }

    fn reset(&self) {
        self.termination.set(Default::default());
        self.error_str.set(String::new());
        self.step_count.set(0);
        self.has_graphics.set(false);
        self.with_current_step_mut(|step| step.reset());
    }

    fn with_current_step_mut<T, F>(&self, func: F) -> T
    where
        F: FnOnce(&mut ExecutionStep) -> T,
    {
        self.steps[self.step_count.get()].with_mut(|step| func(step))
    }

    fn commit_step(&self) {
        debug_assert!(self.step_count.get() < MAX_STEP_COUNT);
        self.step_count.set(self.step_count.get() + 1);
        self.with_current_step_mut(|step| step.reset());
    }

    fn with_completed_step<T, F>(&self, idx: usize, func: F) -> T
    where
        F: FnOnce(&ExecutionStep) -> T,
    {
        debug_assert!(idx < MAX_STEP_COUNT);
        debug_assert!(idx < self.step_count());
        self.steps[idx].with(|step| func(step))
    }

    fn error_string(&self) -> Option<String> {
        if self.termination_type() == TerminationType::Error {
            self.error_str.with(|err| Some(err.clone()))
        } else {
            None
        }
    }

    fn set_error_string(&self, s: String) {
        self.error_str.set(s)
    }

    pub fn wasm_step(&self, line_num: u32) -> bool {
        use TerminationType::*;
        if self.termination_type() == HitBadImport && self.step_count() > 0 {
            self.with_completed_step(self.step_count() - 1, |completed| {
                self.with_current_step_mut(|current| {
                    current.line_num = completed.line_num;
                })
            });
        } else {
            self.with_current_step_mut(|step| step.line_num = line_num);
        }

        self.commit_step();

        match self.termination_type() {
            Running => (),
            TooManySteps => panic!("execution unexpectedly continued after TooManySteps"),
            HitInvalid | HitBadImport => return false,
            Error => panic!("execution unexpectedly continued after Error"),
            Success => panic!("execution unexpectedly continued after Success"),
        }

        // Signal halt
        if self.step_count() >= MAX_STEP_COUNT - 1 {
            self.set_termination_type(TerminationType::TooManySteps);
            return false;
        }

        true
    }

    pub fn graphics_op(&self, act: Action) {
        self.with_current_step_mut(|step| {
            debug_assert!(step.graphics_op.is_none());
            step.graphics_op = Some(act);
        });
        self.has_graphics.set(true);
    }

    pub fn record_slot(&self, val: impl Into<WasmValue>, slot: u32) {
        self.with_current_step_mut(|step| step.slot_assignments.push((slot, val.into())));
    }

    pub fn call_frame_op(&self, op: CallFrameOp) {
        self.with_current_step_mut(|step| step.call_frame_ops.push(op));
    }

    pub fn finish(&self, result: Result<(), String>) -> Result<()> {
        match result {
            Ok(()) => match self.termination_type() {
                TerminationType::Running => self.set_termination_type(TerminationType::Success),
                TerminationType::TooManySteps
                | TerminationType::HitInvalid
                | TerminationType::HitBadImport
                | TerminationType::Success
                | TerminationType::Error => {
                    unreachable!("success after other result");
                }
            },
            Err(err) => match self.termination_type() {
                TerminationType::Running => {
                    if self.step_count() > 0 {
                        self.with_completed_step(self.step_count() - 1, |completed| {
                            self.with_current_step_mut(|current| {
                                current.line_num = completed.line_num;
                            })
                        });
                    }
                    self.commit_step();
                    self.set_termination_type(TerminationType::Error);
                    self.set_error_string(err);
                }
                TerminationType::HitInvalid
                | TerminationType::HitBadImport
                | TerminationType::TooManySteps => {} // error is expected
                TerminationType::Success | TerminationType::Error => {
                    unreachable!("error after other result");
                }
            },
        }
        Ok(())
    }
}

// Debug state stored in Wasm memory
thread_local! {
    pub static RUN_LOG: RunLog = Default::default();
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

#[derive(Clone, Copy, Debug, PartialEq)]
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

#[derive(Debug)]
pub enum CallFrameOp {
    EnterFunc(u32), // local func idx (not including imports)
    BeforeCall,
    AfterCall(u32),
    BeforeTailCall,
    EnterBlock(u32), // block id
}

#[derive(Default, Debug)]
struct ExecutionStep {
    line_num: u32,
    slot_assignments: SmallVec<[(u32, WasmValue); 1]>, // slot idx, value (SmallVec stores in-line if <=1 result)
    graphics_op: Option<Action>,                       // XXX todo: memory
    call_frame_ops: ArrayVec<CallFrameOp, 2>,
}

impl ExecutionStep {
    fn reset(&mut self) {
        self.line_num = 0;
        self.slot_assignments.clear();
        self.graphics_op = None;
        self.call_frame_ops.clear();
    }
}

#[derive(Default)]
pub struct ExecutionStatus {
    pub line_num: Option<usize>,
    pub below_line: bool,
    // after returning from a call (the second step at the same line_num),
    // the arrow is offset to show call has completed (result slots are already written)
    pub termination: TerminationType,
    pub error: Option<String>,
}

#[derive(Copy, Clone)]
pub struct SlotContents {
    // a Wasm ValType, possibly from an old loop iteration or stack frame
    pub val: WasmValue,
    pub old: bool,
}

#[derive(Default)]
pub struct ExecutionState {
    pub next_step: usize,
    pub slots: Vec<SmallVec<[Option<SlotContents>; 1]>>,
    pub status: ExecutionStatus,
    cur_func: Option<u32>,
    call_stack: Vec<(u32, usize)>, // func_idx, height
}

impl ExecutionState {
    pub fn reset(&mut self, cx: &SlotConnections) {
        self.next_step = 0;
        self.status = Default::default();
        self.cur_func = None;
        self.call_stack.clear();
        self.slots.resize(cx.connections.len(), SmallVec::new());
        let first_function_slot = cx
            .first_slot_of_func
            .first()
            .map(|x| x.usize())
            .unwrap_or(self.slots.len());
        for (idx, slot) in self.slots.iter_mut().enumerate() {
            if idx < first_function_slot {
                *slot = smallvec::smallvec![None];
            } else {
                slot.clear();
            }
        }
    }

    pub fn slot_contents(&self, slot_idx: usize) -> Option<SlotContents> {
        *self.slots[slot_idx].last().unwrap()
    }

    pub fn goto_step(
        &mut self,
        log: &RunLog,
        connections: &SlotConnections,
        target: usize,
        mut canvas: Option<&mut DomCanvas>,
    ) {
        if self.next_step > target || self.next_step == 0 {
            // XXX save checkpoints?
            self.reset(connections);
            if let Some(canvas) = canvas.as_mut() {
                canvas.reset(log.has_graphics.get())
            };
        }
        if log.step_count() == 0 {
            return;
        }
        assert!(target < log.step_count());
        while self.next_step <= target {
            log.with_completed_step(self.next_step, |s| {
                for op in &s.call_frame_ops {
                    self.handle_call_frame_op(op, connections);
                }

                for (slot_idx, value) in &s.slot_assignments {
                    *self.slots[*slot_idx as usize].last_mut().unwrap() = Some(SlotContents {
                        val: *value,
                        old: false,
                    });
                }
                if let Some(canvas) = canvas.as_mut() {
                    canvas.render(&s.graphics_op)
                }
            });
            self.next_step += 1;
        }
        log.with_completed_step(target, |s| {
            // top bit: whether arrow should be offset for an "after call" step
            self.status.line_num = Some((s.line_num & 0x7fff_ffff) as usize);
            self.status.below_line = s.line_num & 0x8000_0000 != 0;
        });
        self.status.termination = if target + 1 == log.step_count() {
            log.termination_type()
        } else {
            TerminationType::Running
        };
        self.status.error = log.error_string();
        if self.status.termination == TerminationType::Success {
            debug_assert!(self.call_stack.is_empty());
        }
    }

    // This is more complicated than would be nice. Part of the goal is to avoid
    // materializing a general call stack; we want to show the most useful "slot" values
    // at any given time.

    fn handle_call_frame_op(&mut self, op: &CallFrameOp, connections: &SlotConnections) {
        match op {
            // When re-entering a function: preserve the old slot values, but mark them "old" (grayed out).
            CallFrameOp::EnterFunc(func_idx) => {
                self.cur_func = Some(*func_idx);
                if let Some((lower, upper)) = connections.slot_range(*func_idx) {
                    for slot_idx in lower..upper {
                        let old = self.slots[slot_idx]
                            .last()
                            .copied()
                            .flatten()
                            .map(|SlotContents { val, .. }| SlotContents { val, old: true });
                        self.slots[slot_idx].push(old);
                    }
                }
            }
            // Before a call or call_indirect: store the current depth of this function's slots
            // so they can be restored to this call frame on return.
            CallFrameOp::BeforeCall => {
                if let Some((lower, _upper)) = connections.slot_range(self.cur_func.unwrap()) {
                    self.call_stack
                        .push((self.cur_func.unwrap(), self.slots[lower].len()));
                }
            }
            // Before a tail call: just copy our slots to the previous call frame of this function
            // if there is one. (The goal is to avoid unbounded stack growth.)
            CallFrameOp::BeforeTailCall => {
                if let Some((lower, upper)) = connections.slot_range(self.cur_func.unwrap()) {
                    let first_slot_len = self.slots[lower].len();
                    if first_slot_len > 1 {
                        for slot_idx in lower..upper {
                            let cur = self.slots[slot_idx].pop().unwrap();
                            *self.slots[slot_idx].last_mut().unwrap() = cur;
                        }
                    }
                }
            }
            // After a call or call_indirect: restore the previous depth of this function's slots.
            CallFrameOp::AfterCall(func_idx) => {
                self.cur_func = Some(*func_idx);

                if let Some((lower, upper)) = connections.slot_range(*func_idx) {
                    let (orig_func, target_height) = self.call_stack.pop().unwrap();
                    assert_eq!(orig_func, *func_idx);
                    let first_slot_len = self.slots[lower].len();
                    debug_assert!(first_slot_len >= target_height);

                    for slot_idx in lower..upper {
                        debug_assert_eq!(self.slots[slot_idx].len(), first_slot_len);
                        self.slots[slot_idx].truncate(target_height);
                    }
                }
            }
            // When entering a loop (this is only called for loops but would work fine
            // on any kind of block, but wouldn't do anything useful): mark the previous
            // contents of the slots as "old".
            CallFrameOp::EnterBlock(block_idx) => {
                let (lower, upper) = connections.blocks[*block_idx as usize];
                for slot_idx in lower.usize()..upper.usize() {
                    let cur_but_old = self.slots[slot_idx]
                        .last()
                        .copied()
                        .flatten()
                        .map(|SlotContents { val, .. }| SlotContents { val, old: true });
                    *self.slots[slot_idx].last_mut().unwrap() = cur_but_old;
                }
            }
        }
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

    {
        let codillon_debug = Object::new();

        // Updating the debug state at every step. Returns whether execution should continue.
        let step_closure: ScopedClosure<'_, dyn Fn(u32) -> bool> =
            Closure::new(|line_num: u32| RUN_LOG.with(|r| r.wasm_step(line_num)));
        register_closure(&codillon_debug, "record_step", step_closure);

        let record_invalid: ScopedClosure<'_, dyn Fn()> =
            Closure::new(|| RUN_LOG.with(|r| r.set_termination_type(TerminationType::HitInvalid)));
        register_closure(&codillon_debug, "record_invalid", record_invalid);

        let func_placeholder: ScopedClosure<'_, dyn Fn()> = Closure::new(|| {
            RUN_LOG.with(|r| r.set_termination_type(TerminationType::HitBadImport))
        });
        register_closure(&codillon_debug, "func_placeholder", func_placeholder);

        create_closure_record_operations(&codillon_debug);
        create_closure_frame_operations(&codillon_debug);

        Reflect::set(&imports, &"codillon_debug".into(), &codillon_debug)?;
    }

    {
        let draw = Object::new();
        create_graphics_helpers(&draw);
        Reflect::set(&imports, &"draw".into(), &draw)?;
    }

    Ok(imports)
}

fn create_graphics_helpers(draw: &Object) {
    let draw_point: ScopedClosure<'_, dyn Fn(f64, f64)> =
        Closure::new(|x, y| RUN_LOG.with(|r| r.graphics_op(Action::Point(x, y))));
    register_closure(draw, "point", draw_point);

    let clear_canvas: ScopedClosure<'_, dyn Fn()> =
        Closure::new(|| RUN_LOG.with(|r| r.graphics_op(Action::Clear)));
    register_closure(draw, "clear", clear_canvas);

    let set_color: ScopedClosure<'_, dyn Fn(i32, i32, i32)> =
        Closure::new(|r, g, b| RUN_LOG.with(|log| log.graphics_op(Action::Color(r, g, b))));
    register_closure(draw, "set_color", set_color);

    let set_extent: ScopedClosure<'_, dyn Fn(f64, f64, f64, f64)> =
        Closure::new(|xmin, xmax, ymin, ymax| {
            RUN_LOG.with(|r| r.graphics_op(Action::Extent(xmin, xmax, ymin, ymax)))
        });
    register_closure(draw, "set_extent", set_extent);

    let set_radius: ScopedClosure<'_, dyn Fn(f64)> =
        Closure::new(|radius| RUN_LOG.with(|r| r.graphics_op(Action::Radius(radius))));
    register_closure(draw, "set_radius", set_radius);
}

fn create_closure_record_operations(obj: &Object) {
    let record_i32: ScopedClosure<'_, dyn Fn(i32, u32)> =
        Closure::new(|value, slot| RUN_LOG.with(|r| r.record_slot(value, slot)));
    register_closure(obj, "record_i32", record_i32);

    let record_f32: ScopedClosure<'_, dyn Fn(f32, u32)> =
        Closure::new(|value, slot| RUN_LOG.with(|r| r.record_slot(value, slot)));
    register_closure(obj, "record_f32", record_f32);

    let record_i64: ScopedClosure<'_, dyn Fn(i64, u32)> =
        Closure::new(|value, slot| RUN_LOG.with(|r| r.record_slot(value, slot)));
    register_closure(obj, "record_i64", record_i64);

    let record_f64: ScopedClosure<'_, dyn Fn(f64, u32)> =
        Closure::new(|value, slot| RUN_LOG.with(|r| r.record_slot(value, slot)));
    register_closure(obj, "record_f64", record_f64);
}

fn create_closure_frame_operations(obj: &Object) {
    let enter_func: ScopedClosure<'_, dyn Fn(u32)> = Closure::new(|func_idx| {
        RUN_LOG.with(|r| r.call_frame_op(CallFrameOp::EnterFunc(func_idx)))
    });
    register_closure(obj, "enter_func", enter_func);

    let before_call: ScopedClosure<'_, dyn Fn()> =
        Closure::new(|| RUN_LOG.with(|r| r.call_frame_op(CallFrameOp::BeforeCall)));
    register_closure(obj, "before_call", before_call);

    let after_call: ScopedClosure<'_, dyn Fn(u32)> = Closure::new(|func_idx| {
        RUN_LOG.with(|r| r.call_frame_op(CallFrameOp::AfterCall(func_idx)))
    });
    register_closure(obj, "after_call", after_call);

    let before_tail_call: ScopedClosure<'_, dyn Fn()> =
        Closure::new(|| RUN_LOG.with(|r| r.call_frame_op(CallFrameOp::BeforeTailCall)));
    register_closure(obj, "before_tail_call", before_tail_call);

    let enter_block: ScopedClosure<'_, dyn Fn(u32)> = Closure::new(|block_idx| {
        RUN_LOG.with(|r| r.call_frame_op(CallFrameOp::EnterBlock(block_idx)))
    });
    register_closure(obj, "enter_block", enter_block);
}

pub async fn run_binary(binary: &[u8]) -> Result<()> {
    use js_sys::{Function, Reflect};
    RUN_LOG.with(|r| r.reset());
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
            let res = main.apply(&JsValue::null(), &js_sys::Array::new()); // run the function
            RUN_LOG.with(|log| match res {
                Ok(val) if val.is_undefined() => log.finish(Ok(())),
                Ok(val) => bail!("unhandled return value from function: {:?}", val),
                Err(e) => log.finish(Err(extract_error(&e))),
            })
        }
        Err(e) => bail!("reflection failure: {:?}", e),
    }
}

fn extract_error(jsv: &JsValue) -> String {
    if let Some(r) = jsv.dyn_ref::<RuntimeError>() {
        r.message().into()
    } else {
        let re = Regex::new(r"\((.*)").unwrap();
        let debug_string = format!("{jsv:?}");
        if let Some(captures) = re.captures(&debug_string) {
            captures[1].to_string()
        } else {
            "unknown error".to_string()
        }
    }
}
