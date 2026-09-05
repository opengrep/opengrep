package main

func test_direct_flow_sanitizing_lambda() {
	// ruleid: test-hof-callback-taint
	sink(app_with_direct_flow(func(x string) string { return "3" }, source()))
}
