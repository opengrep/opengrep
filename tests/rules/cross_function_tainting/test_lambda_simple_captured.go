package main

// Test: Simple lambda with captured variable
func test1() {
	x := source()
	callback := func() {
		// ruleid: test-lambda-simple-captured
		sink(x)
	}
	callback()
}

func source() string { return "tainted" }
func sink(s string) {}
