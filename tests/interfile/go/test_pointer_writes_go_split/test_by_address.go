package main

func testByAddress() {
	t := T{}
	setf(&t)
	// ruleid: test-pointer-writes-split
	sink(t.F)
}
