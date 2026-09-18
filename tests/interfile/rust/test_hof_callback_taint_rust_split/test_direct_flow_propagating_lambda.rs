use crate::sink::sink;
use crate::source::source;
use crate::zz_main::app_with_direct_flow;

fn test_direct_flow_propagating_lambda() {
    // ruleid: test-hof-callback-taint
    sink(app_with_direct_flow(|x| x, source()));
}
