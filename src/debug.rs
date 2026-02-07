// Debug manager for time-travel instrumentation
// Currently support:
// - all normal scalar value ops: i32, i64, f32, f64 (pushes from instructions are recorded)
// - memory store ops: i32, i64, f32, f64 (stores record addr + value)
// - local/global set ops for i32
// Currently do not support:
// - other memory operations
// - all SIMD/vector operations
// - all reference types operations
// - call tracking (dynamic function types)

use js_sys::{Array, Object, Reflect};
use std::cell::RefCell;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use web_sys::console::log_1;

const MAX_STEP_COUNT: usize = 10_000;

// Debug state stored in Wasm memory
thread_local! {
    static STATE: RefCell<DebugState> = RefCell::new(DebugState::new());
}

#[derive(Clone, Copy)]
pub enum WebAssemblyTypes {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    V128(u128),
}

impl From<i32> for WebAssemblyTypes {
    fn from(v: i32) -> Self {
        WebAssemblyTypes::I32(v)
    }
}
impl From<i64> for WebAssemblyTypes {
    fn from(v: i64) -> Self {
        WebAssemblyTypes::I64(v)
    }
}
impl From<f32> for WebAssemblyTypes {
    fn from(v: f32) -> Self {
        WebAssemblyTypes::F32(v)
    }
}
impl From<f64> for WebAssemblyTypes {
    fn from(v: f64) -> Self {
        WebAssemblyTypes::F64(v)
    }
}
impl From<u128> for WebAssemblyTypes {
    fn from(v: u128) -> Self {
        WebAssemblyTypes::V128(v)
    }
}

pub struct Change {
    pub line_number: i32,
    pub stack_pushes: Vec<WebAssemblyTypes>,
    pub locals_change: Option<Vec<(usize, WebAssemblyTypes)>>,
    pub globals_change: Option<(u32, WebAssemblyTypes)>,
    pub memory_change: Option<(u32, WebAssemblyTypes)>,
    pub num_pops: u32,
}

struct DebugState {
    // Current State
    stack_pushes: Vec<WebAssemblyTypes>,
    // Vector of local variable indices and values
    locals_change: Option<Vec<(usize, WebAssemblyTypes)>>,
    globals_change: Option<(u32, WebAssemblyTypes)>,
    memory_change: Option<(u32, WebAssemblyTypes)>,
    num_pops: u32,
    func_locals: Vec<Vec<WebAssemblyTypes>>,
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
            func_locals: Vec::new(),
            changes: Vec::new(),
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

// Reset DebugState for new runs
pub fn reset_debug_state() {
    STATE.with(|cur_state| *cur_state.borrow_mut() = DebugState::new());
}

// Constructs the instrumentation functions for import
pub fn make_imports() -> Result<Object, JsValue> {
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
                globals_change: state.globals_change,
                memory_change: state.memory_change,
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
    register_closure(&debug_numbers, "step", step_closure);

    create_closure_local_operations(&debug_numbers);
    create_closure_global_operations(&debug_numbers);
    create_closure_memory_operations(&debug_numbers);

    let push_i32 = Closure::wrap(Box::new(move |value: i32| {
        STATE.with(|cur_state| {
            cur_state
                .borrow_mut()
                .stack_pushes
                .push(WebAssemblyTypes::I32(value));
        });
        value
    }) as Box<dyn Fn(i32) -> i32>);
    register_closure(&debug_numbers, "push_i32", push_i32);

    let push_f32 = Closure::wrap(Box::new(move |value: f32| {
        STATE.with(|cur_state| {
            cur_state
                .borrow_mut()
                .stack_pushes
                .push(WebAssemblyTypes::F32(value));
        });
        value
    }) as Box<dyn Fn(f32) -> f32>);
    register_closure(&debug_numbers, "push_f32", push_f32);

    let push_i64 = Closure::wrap(Box::new(move |value: i64| {
        STATE.with(|cur_state| {
            cur_state
                .borrow_mut()
                .stack_pushes
                .push(WebAssemblyTypes::I64(value));
        });
        value
    }) as Box<dyn Fn(i64) -> i64>);
    register_closure(&debug_numbers, "push_i64", push_i64);

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
    register_closure(&debug_numbers, "push_f64", push_f64);

    let pop_i = Closure::wrap(Box::new(move |pop_num: i32| {
        STATE.with(|cur_state| {
            cur_state.borrow_mut().num_pops = pop_num as u32;
        });
    }) as Box<dyn Fn(i32)>);
    register_closure(&debug_numbers, "pop_i", pop_i);

    Reflect::set(
        &imports,
        &JsValue::from_str("codillon_debug"),
        &debug_numbers,
    )?;
    Ok(imports)
}

fn create_closure_global_operations(debug_numbers: &Object) {
    let set_global_i32 = Closure::wrap(Box::new(move |idx: i32, value: i32| {
        STATE.with(|cur_state| {
            cur_state.borrow_mut().globals_change =
                Some((idx as u32, WebAssemblyTypes::I32(value)));
        });
    }) as Box<dyn Fn(i32, i32)>);
    register_closure(debug_numbers, "set_global_i32", set_global_i32);

    let set_global_f32 = Closure::wrap(Box::new(move |idx: i32, value: f32| {
        STATE.with(|cur_state| {
            cur_state.borrow_mut().globals_change =
                Some((idx as u32, WebAssemblyTypes::F32(value)));
        });
    }) as Box<dyn Fn(i32, f32)>);
    register_closure(debug_numbers, "set_global_f32", set_global_f32);

    let set_global_i64 = Closure::wrap(Box::new(move |idx: i32, value: i64| {
        STATE.with(|cur_state| {
            cur_state.borrow_mut().globals_change =
                Some((idx as u32, WebAssemblyTypes::I64(value)));
        });
    }) as Box<dyn Fn(i32, i64)>);
    register_closure(debug_numbers, "set_global_i64", set_global_i64);

    let set_global_f64 = Closure::wrap(Box::new(move |idx: i32, value: f64| {
        STATE.with(|cur_state| {
            cur_state.borrow_mut().globals_change =
                Some((idx as u32, WebAssemblyTypes::F64(value)));
        });
    }) as Box<dyn Fn(i32, f64)>);
    register_closure(debug_numbers, "set_global_f64", set_global_f64);
}

fn create_closure_local_operations(debug_numbers: &Object) {
    let record_local_change = |idx: usize, value: WebAssemblyTypes| {
        STATE.with(|cur_state| {
            let mut state = cur_state.borrow_mut();
            if let Some(cur_locals) = &mut state.locals_change {
                cur_locals.push((idx, value));
            } else {
                state.locals_change = Some(vec![(idx, value)]);
            }
        });
    };
    let set_local_i32 = Closure::wrap(Box::new(move |idx: i32, value: i32| {
        record_local_change(idx as usize, value.into());
    }) as Box<dyn Fn(i32, i32)>);
    register_closure(debug_numbers, "set_local_i32", set_local_i32);

    let set_local_f32 = Closure::wrap(Box::new(move |idx: i32, value: f32| {
        record_local_change(idx as usize, value.into());
    }) as Box<dyn Fn(i32, f32)>);
    register_closure(debug_numbers, "set_local_f32", set_local_f32);

    let set_local_i64 = Closure::wrap(Box::new(move |idx: i32, value: i64| {
        record_local_change(idx as usize, value.into());
    }) as Box<dyn Fn(i32, i64)>);
    register_closure(debug_numbers, "set_local_i64", set_local_i64);

    let set_local_f64 = Closure::wrap(Box::new(move |idx: i32, value: f64| {
        record_local_change(idx as usize, value.into());
    }) as Box<dyn Fn(i32, f64)>);
    register_closure(debug_numbers, "set_local_f64", set_local_f64);
}

fn create_closure_memory_operations(debug_numbers: &Object) {
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
    register_closure(debug_numbers, "set_memory_i32", set_memory_i32);

    let set_memory_f32 = Closure::wrap(Box::new(move |addr: i32, value: f32| {
        STATE.with(|cur_state| {
            cur_state.borrow_mut().memory_change =
                Some((addr as u32, WebAssemblyTypes::F32(value)));
        });
        let arr = js_sys::Array::new();
        arr.push(&JsValue::from_f64(addr as f64));
        arr.push(&JsValue::from_f64(value as f64));
        arr
    }) as Box<dyn Fn(i32, f32) -> js_sys::Array>);
    register_closure(debug_numbers, "set_memory_f32", set_memory_f32);

    let set_memory_i64 = Closure::wrap(Box::new(move |addr: i32, value: i64| {
        STATE.with(|cur_state| {
            cur_state.borrow_mut().memory_change =
                Some((addr as u32, WebAssemblyTypes::I64(value)));
        });
        let arr = js_sys::Array::new();
        arr.push(&JsValue::from_f64(addr as f64));
        arr.push(&JsValue::from_f64(value as f64));
        arr
    }) as Box<dyn Fn(i32, i64) -> js_sys::Array>);
    register_closure(debug_numbers, "set_memory_i64", set_memory_i64);

    let set_memory_f64 = Closure::wrap(Box::new(move |addr: i32, value: f64| {
        STATE.with(|cur_state| {
            cur_state.borrow_mut().memory_change =
                Some((addr as u32, WebAssemblyTypes::F64(value)));
        });
        let arr = js_sys::Array::new();
        arr.push(&JsValue::from_f64(addr as f64));
        arr.push(&JsValue::from_f64(value));
        arr
    }) as Box<dyn Fn(i32, f64) -> js_sys::Array>);
    register_closure(debug_numbers, "set_memory_f64", set_memory_f64);
}

fn valtype_default(ty: &wasmparser::ValType) -> WebAssemblyTypes {
    match ty {
        wasmparser::ValType::I32 => WebAssemblyTypes::I32(0),
        wasmparser::ValType::I64 => WebAssemblyTypes::I64(0),
        wasmparser::ValType::F32 => WebAssemblyTypes::F32(0.0),
        wasmparser::ValType::F64 => WebAssemblyTypes::F64(0.0),
        wasmparser::ValType::V128 => WebAssemblyTypes::V128(0u128),
        _ => panic!("unsupported valtype for locals"),
    }
}

pub fn initialize_locals(
    params: &Vec<wasmparser::ValType>,
    locals: &Vec<(u32, wasmparser::ValType)>,
) {
    STATE.with(|cur_state| {
        let mut cur_locals: Vec<WebAssemblyTypes> = Vec::new();
        for param in params {
            cur_locals.push(valtype_default(param));
        }
        for (count, local_type) in locals {
            for _ in 0..*count {
                cur_locals.push(valtype_default(local_type));
            }
        }
        cur_state.borrow_mut().func_locals.push(cur_locals);
    });
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

pub fn get_all_locals() -> Vec<Vec<WebAssemblyTypes>> {
    STATE.with(|cur_state| cur_state.borrow().func_locals.clone())
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
    Reflect::set(output, &JsValue::from_str(label), &JsValue::from_f64(num)).ok();
}

fn set_js_vec(output: &Object, label: &str, vector: &Vec<WebAssemblyTypes>) {
    Reflect::set(output, &JsValue::from_str(label), &vec_to_array(vector)).ok();
}

pub fn program_state_to_js(ps: &crate::editor::ProgramState) -> String {
    let output = Object::new();
    set_js_num(&output, "step#", ps.step_number as f64);
    set_js_vec(&output, "stack", &ps.stack_state);
    set_js_vec(&output, "locals", &ps.locals_state);
    set_js_vec(&output, "globals", &ps.globals_state);
    set_js_vec(&output, "mem", &ps.memory_state);
    js_sys::JSON::stringify(&JsValue::from(output))
        .ok()
        .and_then(|s| s.as_string())
        .unwrap_or_else(|| "{}".to_string())
}
