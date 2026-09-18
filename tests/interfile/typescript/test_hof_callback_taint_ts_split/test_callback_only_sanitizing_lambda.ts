function test_callback_only_sanitizing_lambda(): void {
    // ok: test-hof-callback-taint
    sink(app_callback_only((x: string) => 3, source()));
}
