function test_direct_flow_sanitizing_lambda(): void {
    // ruleid: test-hof-callback-taint
    sink(app_with_direct_flow((x: string) => "safe", source()));
}
