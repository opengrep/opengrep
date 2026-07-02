package main

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
