mod get_history;
mod sink;
mod source;
mod test_builtin_filter;
mod test_builtin_for_each;
mod test_builtin_map;
mod test_custom_foreach;
mod test_custom_map;
mod test_direct_call;
mod test_original_example;
mod zz_main;

use crate::test_builtin_filter::test_builtin_filter;
use crate::test_builtin_for_each::test_builtin_for_each;
use crate::test_builtin_map::test_builtin_map;
use crate::test_custom_foreach::test_custom_foreach;
use crate::test_custom_map::test_custom_map;
use crate::test_direct_call::test_direct_call;
use crate::test_original_example::test_original_example;

fn main() {
    test_custom_map();
    test_custom_foreach();
    test_direct_call();
    test_builtin_map();
    test_builtin_for_each();
    test_builtin_filter();
    test_original_example();
}
