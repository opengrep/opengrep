package main

func test_callback_only_propagating_named() {
	// ruleid: test-hof-callback-taint
	sink(app_callback_only(propagates, source()))
}
