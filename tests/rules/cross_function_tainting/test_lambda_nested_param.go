package main

// Test: Nested lambda with param at each level
func test5() {
	outer := func(a string) {
		inner := func(b string) {
			// ruleid: test-lambda-nested-param
			sink(b)
		}
		inner(a)
	}
	outer(source())
}

func source() string { return "tainted" }
func sink(s string) {}
