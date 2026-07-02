fn test_callback_only_sanitizing_lambda() {
    // ok: test-hof-callback-taint
    sink(app_callback_only(|_x| "3".to_string(), source()));
}
