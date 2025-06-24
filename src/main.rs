mod utils;

fn main() {
    is_well_formed_instr_test();
    leptos::mount::mount_to_body(|| view! { <p>"Hello, world!"</p> })
}

///testing is_well_formed_instr function
///
///prints sample instruction and bool
fn is_well_formed_instr_test() {
    let sample_instr = "i32.add";
    println!(
        "\"{}\" is well-formed: {}",
        sample_instr,
        utils::is_well_formed_instr(sample_instr)
    );
}
