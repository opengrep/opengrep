package main

// Test: No taint - should have NO findings
func test6() {
	x := "clean"
	callback := func() {
		// ok: test-lambda-no-taint
		sink(x)
	}
	callback()
}

func source() string { return "tainted" }
func sink(s string) {}
