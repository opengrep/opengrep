package main

// Test: Deeply nested lambdas (3 levels)
func test4() {
	x := source()
	level1 := func() {
		level2 := func() {
			level3 := func() {
				// ruleid: test-lambda-deeply-nested
				sink(x)
			}
			level3()
		}
		level2()
	}
	level1()
}

func source() string { return "tainted" }
func sink(s string) {}
