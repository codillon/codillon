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
    line_number: i32,
    stack_pushes: Vec<WebAssemblyTypes>,
    locals_change: Option<(u32, WebAssemblyTypes)>,
    globals_change: Option<(u32, WebAssemblyTypes)>,
    num_pops: u32,
}

struct DebugState {
    // Current State
    stack_pushes: Vec<WebAssemblyTypes>,
    locals_change: Option<(u32, WebAssemblyTypes)>,
    globals_change: Option<(u32, WebAssemblyTypes)>,
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
                num_pops: state.num_pops,
            };
            state.changes.push(cur_change);
            state.stack_pushes.clear();
            state.locals_change = None;
            state.globals_change = None;
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

// Converts snapshot and reconstructs locals and globals from changes
pub fn get_state_js(step_idx: usize) -> JsValue {
    if step_idx > MAX_STEP_COUNT || step_idx > last_step() {
        return JsValue::NULL;
    }
    STATE.with(|cur_state| {
        let state = cur_state.borrow();
        let output = Object::new();
        let mut stack = Vec::new();
        let mut locals = Vec::new();
        let mut globals = Vec::new();
        // Changes need to be reconstructed
        for i in 0..=step_idx {
            let change = &state.changes[i];
            if change.num_pops > 0 {
                stack.truncate(stack.len().saturating_sub(change.num_pops as usize));
            }
            stack.append(&mut change.stack_pushes.clone());
            if change.locals_change.is_some() {
                let (local_idx, local_value) = change.locals_change.clone().unwrap();
                locals.resize((local_idx + 1) as usize, WebAssemblyTypes::I32(0));
                locals[local_idx as usize] = local_value;
            }
            if change.globals_change.is_some() {
                let (global_idx, global_value) = change.globals_change.clone().unwrap();
                globals.resize((global_idx + 1) as usize, WebAssemblyTypes::I32(0));
                globals[global_idx as usize] = global_value;
            }
        }
        Reflect::set(
            &output,
            &JsValue::from_str("line_number"),
            &JsValue::from_f64(state.changes[step_idx].line_number as f64),
        )
        .ok();
        Reflect::set(&output, &JsValue::from_str("stack"), &vec_to_array(&stack)).ok();
        Reflect::set(
            &output,
            &JsValue::from_str("locals"),
            &vec_to_array(&locals),
        )
        .ok();
        Reflect::set(
            &output,
            &JsValue::from_str("globals"),
            &vec_to_array(&globals),
        )
        .ok();
        JsValue::from(output)
    })
}

pub fn get_all_changes() -> JsValue {
    STATE.with(|cur_state| {
        let state = cur_state.borrow();
        let result = Array::new();
        // Changes need to be reconstructed
        for change in &state.changes {
            let output = Object::new();
            Reflect::set(
                &output,
                &JsValue::from_str("line_number"),
                &JsValue::from_f64(change.line_number as f64),
            )
            .ok();
            Reflect::set(
                &output,
                &JsValue::from_str("stack_pushes"),
                &vec_to_array(&change.stack_pushes),
            )
            .ok();
            Reflect::set(
                &output,
                &JsValue::from_str("num_pops"),
                &JsValue::from_f64(change.num_pops as f64),
            )
            .ok();
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
                Reflect::set(&output, &JsValue::from_str("locals_change"), &local_obj).ok();
            } else {
                Reflect::set(&output, &JsValue::from_str("locals_change"), &JsValue::NULL).ok();
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
                Reflect::set(&output, &JsValue::from_str("globals_change"), &global_obj).ok();
            } else {
                Reflect::set(
                    &output,
                    &JsValue::from_str("globals_change"),
                    &JsValue::NULL,
                )
                .ok();
            }
            result.push(&output);
        }
        JsValue::from(result)
    })
}
