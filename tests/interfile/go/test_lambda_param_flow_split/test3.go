package main

func test3() {
	callback := func(x string) {
		// ruleid: test-lambda-param-flow
		sink(x)
	}
	callback(source())
}
