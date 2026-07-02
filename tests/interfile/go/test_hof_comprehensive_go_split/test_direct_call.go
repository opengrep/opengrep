package main

func test_direct_call() {
	directCall(func(x string) {
		// ruleid: test-hof-taint
		sink(x)
	})
}
