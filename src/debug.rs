// Debug manager for time-travel instrumentation

use js_sys::{Array, Object, Reflect};
use std::cell::RefCell;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::console::log_1;

const MAX_STEP_COUNT: usize = 100;
const CHECKPOINT_INTERVAL: usize = 300;

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
}

struct Snapshot {
    line: i32,
    stack: Vec<WebAssemblyTypes>,
    locals_changes: Vec<(u32, WebAssemblyTypes)>,
    globals_changes: Vec<(u32, WebAssemblyTypes)>,

    // Only stored at checkpoints
    locals: Option<Vec<WebAssemblyTypes>>,
    globals: Option<Vec<WebAssemblyTypes>>,
}

struct DebugState {
    // Current State
    step: usize,
    stack: Vec<WebAssemblyTypes>,
    locals: Vec<WebAssemblyTypes>,
    globals: Vec<WebAssemblyTypes>,

    // Not yet snapshotted changes
    pending_locals: Vec<(u32, WebAssemblyTypes)>,
    pending_globals: Vec<(u32, WebAssemblyTypes)>,

    // Chronological per-step snapshots
    snapshots: Vec<Snapshot>
}

impl DebugState {
    fn new() -> Self {
        Self {
            step: 0,
            stack: Vec::new(),
            locals: Vec::new(),
            globals: Vec::new(),
            pending_locals: Vec::new(),
            pending_globals: Vec::new(),
            snapshots: Vec::new(),
        }
    }
}
// Constructs the import object `{ codillon_debug: { ... } }`.
pub fn make_imports() -> Result<Object, JsValue> {
    let imports = Object::new();
    let debug_numbers = Object::new();

    // Updating the debug state at every step
    let step_closure = Closure::wrap(Box::new(move |line_num: i32| {
        STATE.with(|cur_state| {
            let mut state = cur_state.borrow_mut();
            if state.step > MAX_STEP_COUNT {
                log_1(&"debug: max step count exceeded".into());
                wasm_bindgen::throw_val(JsValue::from_str("max step count exceeded"));
            }
            let checkpoint = state.snapshots.len() % CHECKPOINT_INTERVAL == 0;
            if checkpoint {
                // Safe to clear pending changes
                state.pending_locals.clear();
                state.pending_globals.clear();
            }
            let snapshot = if checkpoint {
                Snapshot {
                    line: line_num,
                    stack: state.stack.clone(),
                    locals_changes: state.pending_locals.clone(),
                    globals_changes: state.pending_globals.clone(),
                    locals: Some(state.locals.clone()),
                    globals: Some(state.globals.clone()),
                }
            } else {
                Snapshot {
                    line: line_num,
                    stack: state.stack.clone(),
                    locals_changes: state.pending_locals.clone(),
                    globals_changes: state.pending_globals.clone(),
                    // Avoid repeated storage
                    locals: None,
                    globals: None,
                }
            };
            state.snapshots.push(snapshot);
            state.step = state.step.saturating_add(1);
        })
    }) as Box<dyn Fn(i32)>);
    Reflect::set(&debug_numbers, &JsValue::from_str("step"), step_closure.as_ref().unchecked_ref()).ok();
    step_closure.forget();

    let set_local = Closure::wrap(Box::new(move |idx: i32, value: i32| {
        STATE.with(|cur_state| {
            let mut state = cur_state.borrow_mut();
            if idx as usize >= state.locals.len() {
                state.locals.resize((idx + 1) as usize, WebAssemblyTypes::I32(0));
            }
            state.locals[idx as usize] = WebAssemblyTypes::I32(value);
            state.pending_locals.push((idx as u32, WebAssemblyTypes::I32(value)));
        });
    }) as Box<dyn Fn(i32, i32)>);
    Reflect::set(&debug_numbers, &JsValue::from_str("set_local"), set_local.as_ref().unchecked_ref()).ok();
    set_local.forget();

    let set_global = Closure::wrap(Box::new(move |idx: i32, value: i32| {
        STATE.with(|cur_state| {
            let mut state = cur_state.borrow_mut();
            if idx as usize >= state.globals.len() {
                state.globals.resize((idx + 1) as usize, WebAssemblyTypes::I32(0));
            }
            state.globals[idx as usize] = WebAssemblyTypes::I32(value);
            state.pending_globals.push((idx as u32, WebAssemblyTypes::I32(value)));
        });
    }) as Box<dyn Fn(i32, i32)>);
    Reflect::set(&debug_numbers, &JsValue::from_str("set_global"), set_global.as_ref().unchecked_ref()).ok();
    set_global.forget();

    // Store i32.const expressions
    let push_i32 = Closure::wrap(Box::new(move |value: i32| {
        STATE.with(|cur_state| { 
            cur_state.borrow_mut().stack.push(WebAssemblyTypes::I32(value));
        });
    }) as Box<dyn Fn(i32)>);
    Reflect::set(&debug_numbers, &JsValue::from_str("push_i32"), push_i32.as_ref().unchecked_ref()).ok();
    push_i32.forget();

    let pop_one = Closure::wrap(Box::new(move || {
        STATE.with(|cur_state| {
            let mut state = cur_state.borrow_mut();
            state.stack.pop();
        });
    }) as Box<dyn Fn()>);
    Reflect::set(&debug_numbers, &JsValue::from_str("pop_one"), pop_one.as_ref().unchecked_ref())?;
    pop_one.forget();

    Reflect::set(&imports, &JsValue::from_str("codillon_debug"), &debug_numbers)?;
    Ok(imports)
}

#[wasm_bindgen]
pub fn last_step() -> usize {
    STATE.with(|cur_state| cur_state.borrow().step)
}

fn web_assembly_to_string( value: &WebAssemblyTypes ) -> String {
    match value {
        WebAssemblyTypes::I32(v) => format!("i32({})", v),
        WebAssemblyTypes::I64(v) => format!("i64({})", v),
        WebAssemblyTypes::F32(v) => format!("f32({})", v),
        WebAssemblyTypes::F64(v) => format!("f64({})", v),
    }
}

fn vec_to_array( cur_vec: &Vec<WebAssemblyTypes>) ->Array {
    let output_arr = Array::new();
    for value in cur_vec {
        let result = web_assembly_to_string(value);
        output_arr.push(&JsValue::from_str(&result));
    }
    output_arr
}

// Converts snapshot and reconstructs locals and globals from changes
#[wasm_bindgen]
pub fn get_snapshot_js(step_idx: usize) -> JsValue {
    if step_idx > MAX_STEP_COUNT || step_idx > last_step(){
        return JsValue::NULL;
    }

    STATE.with(|cur_state| {
        let state = cur_state.borrow();
        let snapshot = &state.snapshots[step_idx];
        let output = Object::new();
        Reflect::set(&output, &JsValue::from_str("line"), &JsValue::from_f64(snapshot.line as f64)).ok();
        Reflect::set(&output, &JsValue::from_str("stack"), &vec_to_array(&snapshot.stack)).ok();

        // Nearest previous checkpoint
        let checkpoint_idx = (step_idx / CHECKPOINT_INTERVAL) * CHECKPOINT_INTERVAL;
        let checkpoint_snapshot = &state.snapshots[checkpoint_idx];
        let mut local_vals = checkpoint_snapshot.locals.clone().expect(&format!("local empty at checkpoint {}", checkpoint_idx));
        let mut global_vals = checkpoint_snapshot.globals.clone().expect(&format!("global empty at checkpoint {}", checkpoint_idx));
        // Changes need to be reconstructed
        for (local_index, local_value) in snapshot.locals_changes.clone() {
            if (local_index as usize) >= local_vals.len() {
                local_vals.resize((local_index + 1) as usize, WebAssemblyTypes::I32(0));  
            }
            local_vals[local_index as usize] = local_value;
        }
        for (global_index, global_value) in snapshot.globals_changes.clone() {
            if (global_index as usize) >= global_vals.len() {
                global_vals.resize((global_index + 1) as usize, WebAssemblyTypes::I32(0));
            }
            global_vals[global_index as usize] = global_value;
        }
        Reflect::set(&output, &JsValue::from_str("locals"), &vec_to_array(&local_vals)).ok();
        Reflect::set(&output, &JsValue::from_str("globals"), &vec_to_array(&global_vals)).ok();
        JsValue::from(output)
    })
}