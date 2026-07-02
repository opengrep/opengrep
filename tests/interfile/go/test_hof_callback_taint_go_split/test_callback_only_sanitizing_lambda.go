package main

func test_callback_only_sanitizing_lambda() {
	// ok: test-hof-callback-taint
	sink(app_callback_only(func(x string) string { return "3" }, source()))
}
