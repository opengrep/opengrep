use crate::sink::sink;
use crate::source::source;
use crate::zz_main::custom_map;

fn test_custom_map() {
    let tainted = source();
    let arr = vec![tainted];
    custom_map(&arr, |x| {
        // ruleid: test-hof-taint
        sink(x);
        x.clone()
    });
}
