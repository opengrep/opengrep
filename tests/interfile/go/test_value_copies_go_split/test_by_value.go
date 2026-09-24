package main

func testByValue() {
	a := T{}
	byValue(a)
	// ok: test-value-copies-split
	sink(a.F)
	b := &T{}
	byPointer(b)
	// ruleid: test-value-copies-split
	sink(b.F)
}
