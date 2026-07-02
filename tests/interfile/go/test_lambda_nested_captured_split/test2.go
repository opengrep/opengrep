package main

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
