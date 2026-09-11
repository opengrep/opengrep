fn test_callback_only_propagating_lambda() {
    // ruleid: test-hof-callback-taint
    sink(app_callback_only(|x| x, source()));
}
