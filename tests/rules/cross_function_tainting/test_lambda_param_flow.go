package main

// Test: Lambda parameter receives taint at call site
func test3() {
	callback := func(x string) {
		// ruleid: test-lambda-param-flow
		sink(x)
	}
	callback(source())
}

func source() string { return "tainted" }
func sink(s string) {}
