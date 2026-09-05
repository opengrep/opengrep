package main

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
