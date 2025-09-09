fn get_tainted_data() -> String {
    source()
}

fn process_data(data: String) -> String {
    // ruleid: simple_rust_taint
    sink(data)
}

fn main() {
    let tainted_input = get_tainted_data();
    let result = process_data(tainted_input);
}