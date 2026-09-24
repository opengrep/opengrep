package main

func toSink(a string) {
	// ruleid: test-function-reference-go
	sink(a)
}

func viaVariable() {
	f := toSink
	f(source())
}
