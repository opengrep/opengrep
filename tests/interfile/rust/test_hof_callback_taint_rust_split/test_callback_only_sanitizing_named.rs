use crate::sanitizes::sanitizes;
use crate::sink::sink;
use crate::source::source;
use crate::zz_main::app_callback_only;

fn test_callback_only_sanitizing_named() {
    // ok: test-hof-callback-taint
    sink(app_callback_only(sanitizes, source()));
}
