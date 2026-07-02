package main

func test_callback_only_propagating_lambda() {
	// ruleid: test-hof-callback-taint
	sink(app_callback_only(func(x string) string { return x }, source()))
}
