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

// Test: Deeply nested lambdas split across functions
func test4_level1(x string) {
	level2 := func() {
		level3 := func() {
			// ruleid: test-lambda-deeply-nested
			sink(x)
		}
		level3()
	}
	level2()
}

func test4_caller() {
	x := source()
	test4_level1(x)
}

func source() string { return "tainted" }
func sink(s string) {}
