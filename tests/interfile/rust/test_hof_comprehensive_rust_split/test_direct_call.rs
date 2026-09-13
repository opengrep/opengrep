use crate::sink::sink;
use crate::zz_main::direct_call;

fn test_direct_call() {
    direct_call(|x| {
        // ruleid: test-hof-taint
        sink(&x);
    });
}
