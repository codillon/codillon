use std::cell::RefCell;

thread_local! {
    static AUDIO_STATE: RefCell<AudioState> = RefCell::new(AudioState::default());
}

#[derive(Default)]
pub struct AudioState {
    is_running: bool,
}

impl AudioState {
    fn reset(&mut self) {
        self.is_running = true;
    }
}

pub fn reset_audio_state() {
    AUDIO_STATE.with(|state| state.borrow_mut().reset())
}

pub fn is_audio_running() -> bool {
    AUDIO_STATE.with(|state| state.borrow().is_running)
}

pub fn stop_audio() {
    AUDIO_STATE.with_borrow_mut(|state| {
        state.is_running = false;
    });
}

pub async fn run_binary_audio() {
    reset_audio_state();
}
