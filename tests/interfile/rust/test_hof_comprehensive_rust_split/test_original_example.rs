use crate::get_history::get_history;
use crate::sink::sink;
use crate::zz_main::custom_for_each;

fn test_original_example() {
    let history = get_history("name", "owner");
    let arr = vec![history];
    custom_for_each(&arr, |node| {
        let changes = node.clone();
        // ruleid: test-hof-taint
        sink(changes);
    });
}
