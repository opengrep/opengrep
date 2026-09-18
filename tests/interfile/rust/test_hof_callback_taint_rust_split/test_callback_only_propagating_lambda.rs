use crate::sink::sink;
use crate::source::source;
use crate::zz_main::app_callback_only;

fn test_callback_only_propagating_lambda() {
    // ruleid: test-hof-callback-taint
    sink(app_callback_only(|x| x, source()));
}
