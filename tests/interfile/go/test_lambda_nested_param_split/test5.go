package main

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
