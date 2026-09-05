package main

func sink(s string) {}

func source() string { return "" }

func forward(s string) {
	// ruleid: callback-to-unknown-callee
	sink(s)
}

func use(v Visitor) {
	walk(v, source())
}
