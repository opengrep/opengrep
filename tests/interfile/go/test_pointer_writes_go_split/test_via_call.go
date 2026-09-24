package main

func testViaCall() {
	s := ""
	set(&s)
	// ruleid: test-pointer-writes-split
	sink(s)
}
