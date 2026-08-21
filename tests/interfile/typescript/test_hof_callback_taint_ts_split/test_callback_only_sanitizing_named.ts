function test_callback_only_sanitizing_named(): void {
    // ok: test-hof-callback-taint
    sink(app_callback_only(sanitizes, source()));
}
