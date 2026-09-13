use crate::sanitizes::sanitizes;
use crate::sink::sink;
use crate::source::source;
use crate::zz_main::app_with_direct_flow;

fn test_direct_flow_sanitizing_named() {
    // ruleid: test-hof-callback-taint
    sink(app_with_direct_flow(sanitizes, source()));
}
