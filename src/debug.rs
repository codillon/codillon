// Debug manager for time-travel instrumentation

use js_sys::{Array, Object, Reflect};
use std::cell::RefCell;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use web_sys::console::log_1;

const MAX_STEP_COUNT: usize = 1000;

// Debug state stored in Wasm memory
thread_local! {
    static STATE: RefCell<DebugState> = RefCell::new(DebugState::new());
}

#[derive(Clone)]
pub enum WebAssemblyTypes {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    V128(u128),
}

pub struct Change {
    pub line_number: i32,
    pub stack_pushes: Vec<WebAssemblyTypes>,
    pub locals_change: Option<(u32, WebAssemblyTypes)>,
    pub globals_change: Option<(u32, WebAssemblyTypes)>,
    pub memory_change: Option<(u32, WebAssemblyTypes)>,
    pub num_pops: u32,
}

struct DebugState {
    // Current State
    stack_pushes: Vec<WebAssemblyTypes>,
    locals_change: Option<(u32, WebAssemblyTypes)>,
    globals_change: Option<(u32, WebAssemblyTypes)>,
    memory_change: Option<(u32, WebAssemblyTypes)>,
    num_pops: u32,
    // Chronological per-step changes
    changes: Vec<Change>,
}

impl DebugState {
    fn new() -> Self {
        Self {
            stack_pushes: Vec::new(),
            locals_change: None,
            globals_change: None,
            memory_change: None,
            num_pops: 0,
            changes: Vec::new(),
        }
    }
}
// Constructs the instrumentation functions for import
pub fn make_imports() -> Result<Object, JsValue> {
    // Reset DebugState for new runs
    STATE.with(|cur_state| *cur_state.borrow_mut() = DebugState::new());
    let imports = Object::new();
    let debug_numbers = Object::new();

    // Updating the debug state at every step. Return 1 to continue, 0 to halt.
    let step_closure = Closure::wrap(Box::new(move |line_num: i32| -> i32 {
        STATE.with(|cur_state| {
            let mut state = cur_state.borrow_mut();
            // Signal halt
            if state.changes.len() >= MAX_STEP_COUNT {
                log_1(&"debug: max step count exceeded".into());
                return 0;
            }
            let cur_change = Change {
                line_number: line_num,
                stack_pushes: state.stack_pushes.clone(),
                locals_change: state.locals_change.clone(),
                globals_change: state.globals_change.clone(),
                memory_change: state.memory_change.clone(),
                num_pops: state.num_pops,
            };
            state.changes.push(cur_change);
            state.stack_pushes.clear();
            state.locals_change = None;
            state.globals_change = None;
            state.memory_change = None;
            state.num_pops = 0;
            1
        })
    }) as Box<dyn Fn(i32) -> i32>);
    Reflect::set(
        &debug_numbers,
        &JsValue::from_str("step"),
        step_closure.as_ref().unchecked_ref(),
    )
    .ok();
    step_closure.forget();

    let set_local_i32 = Closure::wrap(Box::new(move |idx: i32, value: i32| {
        STATE.with(|cur_state| {
            cur_state.borrow_mut().locals_change = Some((idx as u32, WebAssemblyTypes::I32(value)));
        });
    }) as Box<dyn Fn(i32, i32)>);
    Reflect::set(
        &debug_numbers,
        &JsValue::from_str("set_local_i32"),
        set_local_i32.as_ref().unchecked_ref(),
    )
    .ok();
    set_local_i32.forget();

    let set_global_i32 = Closure::wrap(Box::new(move |idx: i32, value: i32| {
        STATE.with(|cur_state| {
            cur_state.borrow_mut().globals_change =
                Some((idx as u32, WebAssemblyTypes::I32(value)));
        });
    }) as Box<dyn Fn(i32, i32)>);
    Reflect::set(
        &debug_numbers,
        &JsValue::from_str("set_global_i32"),
        set_global_i32.as_ref().unchecked_ref(),
    )
    .ok();
    set_global_i32.forget();

    let set_memory_i32 = Closure::wrap(Box::new(move |addr: i32, value: i32| {
        STATE.with(|cur_state| {
            cur_state.borrow_mut().memory_change =
                Some((addr as u32, WebAssemblyTypes::I32(value)));
        });
        // Return [addr, value]
        let arr = js_sys::Array::new();
        arr.push(&JsValue::from_f64(addr as f64));
        arr.push(&JsValue::from_f64(value as f64));
        arr
    }) as Box<dyn Fn(i32, i32) -> js_sys::Array>);
    Reflect::set(
        &debug_numbers,
        &JsValue::from_str("set_memory_i32"),
        set_memory_i32.as_ref().unchecked_ref(),
    )
    .ok();
    set_memory_i32.forget();

    let set_memory_f32 = Closure::wrap(Box::new(move |addr: i32, value: f32| {
        STATE.with(|cur_state| {
            cur_state.borrow_mut().memory_change =
                Some((addr as u32, WebAssemblyTypes::F32(value)));
        });
        // Return [addr, value]
        let arr = js_sys::Array::new();
        arr.push(&JsValue::from_f64(addr as f64));
        arr.push(&JsValue::from_f64(value as f64));
        arr
    }) as Box<dyn Fn(i32, f32) -> js_sys::Array>);
    Reflect::set(
        &debug_numbers,
        &JsValue::from_str("set_memory_f32"),
        set_memory_f32.as_ref().unchecked_ref(),
    )
    .ok();
    set_memory_f32.forget();

    // Store i32.const expressions
    let push_i32 = Closure::wrap(Box::new(move |value: i32| {
        STATE.with(|cur_state| {
            cur_state
                .borrow_mut()
                .stack_pushes
                .push(WebAssemblyTypes::I32(value));
        });
        value
    }) as Box<dyn Fn(i32) -> i32>);
    Reflect::set(
        &debug_numbers,
        &JsValue::from_str("push_i32"),
        push_i32.as_ref().unchecked_ref(),
    )
    .ok();
    push_i32.forget();

    // Store f32.const expressions
    let push_f32 = Closure::wrap(Box::new(move |value: f32| {
        STATE.with(|cur_state| {
            cur_state
                .borrow_mut()
                .stack_pushes
                .push(WebAssemblyTypes::F32(value));
        });
        value
    }) as Box<dyn Fn(f32) -> f32>);
    Reflect::set(
        &debug_numbers,
        &JsValue::from_str("push_f32"),
        push_f32.as_ref().unchecked_ref(),
    )
    .ok();
    push_f32.forget();

    // Store i64.const expressions
    let push_i64 = Closure::wrap(Box::new(move |value: i64| {
        STATE.with(|cur_state| {
            cur_state
                .borrow_mut()
                .stack_pushes
                .push(WebAssemblyTypes::I64(value));
        });
        value
    }) as Box<dyn Fn(i64) -> i64>);
    Reflect::set(
        &debug_numbers,
        &JsValue::from_str("push_i64"),
        push_i64.as_ref().unchecked_ref(),
    )
    .ok();
    push_i64.forget();

    // Store f64.const expressions
    let push_f64 = Closure::wrap(Box::new(move |value: f64| {
        STATE.with(|cur_state| {
            cur_state
                .borrow_mut()
                .stack_pushes
                .push(WebAssemblyTypes::F64(value));
        });
        value
    }) as Box<dyn Fn(f64) -> f64>);
    Reflect::set(
        &debug_numbers,
        &JsValue::from_str("push_f64"),
        push_f64.as_ref().unchecked_ref(),
    )
    .ok();
    push_f64.forget();

    let pop_i = Closure::wrap(Box::new(move |pop_num: i32| {
        STATE.with(|cur_state| {
            cur_state.borrow_mut().num_pops = pop_num as u32;
        });
    }) as Box<dyn Fn(i32)>);
    Reflect::set(
        &debug_numbers,
        &JsValue::from_str("pop_i"),
        pop_i.as_ref().unchecked_ref(),
    )?;
    pop_i.forget();

    Reflect::set(
        &imports,
        &JsValue::from_str("codillon_debug"),
        &debug_numbers,
    )?;
    Ok(imports)
}

pub fn last_step() -> usize {
    STATE.with(|cur_state| cur_state.borrow().changes.len().saturating_sub(1))
}

pub fn with_changes<T, F>(get_iter: F) -> T
where
    F: FnOnce(std::slice::Iter<'_, Change>) -> T,
{
    STATE.with(|cur_state| get_iter(cur_state.borrow().changes.iter()))
}

fn type_to_string(value: &WebAssemblyTypes) -> String {
    match value {
        WebAssemblyTypes::I32(v) => format!("i32({})", v),
        WebAssemblyTypes::I64(v) => format!("i64({})", v),
        WebAssemblyTypes::F32(v) => format!("f32({})", v),
        WebAssemblyTypes::F64(v) => format!("f64({})", v),
        WebAssemblyTypes::V128(v) => format!("v128({}", v),
    }
}

fn vec_to_array(cur_vec: &Vec<WebAssemblyTypes>) -> Array {
    let output_arr = Array::new();
    for value in cur_vec {
        let result = type_to_string(value);
        output_arr.push(&JsValue::from_str(&result));
    }
    output_arr
}

fn set_js_num(output: &Object, label: &str, num: f64) {
    Reflect::set(
        &output,
        &JsValue::from_str(label),
        &JsValue::from_f64(num),
    ).ok();
}

fn set_js_vec(output: &Object, label: &str, vector: &Vec<WebAssemblyTypes>) {
    Reflect::set(
        &output,
        &JsValue::from_str(label),
        &vec_to_array(&vector),
    ).ok();
}

pub fn change_to_js(change: &Change) -> JsValue {
    let output = Object::new();
    set_js_num(&output, "line#", change.line_number as f64);
    set_js_vec(&output, "stack", &change.stack_pushes);
    set_js_num(&output, "pops", change.num_pops as f64);
    if let Some((idx, val)) = &change.locals_change {
        let local_obj = Object::new();
        Reflect::set(
            &local_obj,
            &JsValue::from_str("idx"),
            &JsValue::from_f64(*idx as f64),
        )
        .ok();
        Reflect::set(
            &local_obj,
            &JsValue::from_str("value"),
            &JsValue::from_str(&type_to_string(val)),
        )
        .ok();
        Reflect::set(&output, &JsValue::from_str("locals"), &local_obj).ok();
    } else {
        Reflect::set(&output, &JsValue::from_str("locals"), &JsValue::NULL).ok();
    }
    if let Some((idx, val)) = &change.globals_change {
        let global_obj = Object::new();
        Reflect::set(
            &global_obj,
            &JsValue::from_str("idx"),
            &JsValue::from_f64(*idx as f64),
        )
        .ok();
        Reflect::set(
            &global_obj,
            &JsValue::from_str("value"),
            &JsValue::from_str(&type_to_string(val)),
        )
        .ok();
        Reflect::set(&output, &JsValue::from_str("globals"), &global_obj).ok();
    } else {
        Reflect::set(&output, &JsValue::from_str("globals"), &JsValue::NULL).ok();
    }
    if let Some((idx, val)) = &change.memory_change {
        let mem_obj = Object::new();
        Reflect::set(
            &mem_obj,
            &JsValue::from_str("idx"),
            &JsValue::from_f64(*idx as f64),
        )
        .ok();
        Reflect::set(
            &mem_obj,
            &JsValue::from_str("value"),
            &JsValue::from_str(&type_to_string(val)),
        )
        .ok();
        Reflect::set(&output, &JsValue::from_str("mem"), &mem_obj).ok();
    } else {
        Reflect::set(&output, &JsValue::from_str("mem"), &JsValue::NULL).ok();
    }
    JsValue::from(output)
}

pub fn program_state_to_js(ps: &crate::editor::ProgramState) -> JsValue {
    let output = Object::new();
    set_js_num(&output, "step#", ps.step_number as f64);
    set_js_num(&output, "line#", ps.line_number as f64);
    set_js_vec(&output, "stack", &ps.stack_state);
    set_js_vec(&output, "locals", &ps.locals_state);
    set_js_vec(&output, "globals", &ps.globals_state);
    set_js_vec(&output, "mem", &ps.memory_state);
    JsValue::from(output)
}
