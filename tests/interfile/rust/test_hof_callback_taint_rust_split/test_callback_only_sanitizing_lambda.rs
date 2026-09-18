use crate::sink::sink;
use crate::source::source;
use crate::zz_main::app_callback_only;

fn test_callback_only_sanitizing_lambda() {
    // ok: test-hof-callback-taint
    sink(app_callback_only(|_x| "3".to_string(), source()));
}
