use crate::sink::sink;
use crate::source::source;
use crate::zz_main::app_with_direct_flow;

fn test_direct_flow_sanitizing_lambda() {
    // ruleid: test-hof-callback-taint
    sink(app_with_direct_flow(|_x| "3".to_string(), source()));
}
