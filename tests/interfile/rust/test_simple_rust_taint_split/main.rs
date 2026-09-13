mod get_tainted_data;
mod process_data;

use crate::get_tainted_data::get_tainted_data;
use crate::process_data::process_data;

fn main() {
    let tainted_input = get_tainted_data();
    let result = process_data(tainted_input);
}