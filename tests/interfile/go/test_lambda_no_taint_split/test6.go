package main

func test6() {
	x := "clean"
	callback := func() {
		// ok: test-lambda-no-taint
		sink(x)
	}
	callback()
}
