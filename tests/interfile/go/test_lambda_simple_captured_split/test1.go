package main

func test1() {
	x := source()
	callback := func() {
		// ruleid: test-lambda-simple-captured
		sink(x)
	}
	callback()
}
