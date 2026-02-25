package main

// Test: Nested lambda capturing parent lambda's parameter
func test2() {
	outer := func(a string) {
		inner := func() {
			// ruleid: test-lambda-nested-captured
			sink(a)
		}
		inner()
	}
	outer(source())
}

func source() string { return "tainted" }
func sink(s string) {}
