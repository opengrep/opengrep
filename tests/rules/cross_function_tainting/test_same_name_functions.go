package main

func test(input string) {
	var fn = func(s string) {
		// ruleid: taint-func-param
		sink(s)
	}
	fn("")
}

func test(input string) {
	var fn = func(s string) {
		// ok: taint-func-param
		sink(s)
	}
}
