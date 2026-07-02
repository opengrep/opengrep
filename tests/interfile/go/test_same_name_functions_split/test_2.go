package main

func test(input string) {
	var fn = func(s string) {
		// ok: taint-func-param
		safe(s)
	}
	fn("")
}
