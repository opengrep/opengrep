package main

func testReceivers() {
	c := T{}
	c.setV()
	// ok: test-value-copies-split
	sink(c.F)
	d := &T{}
	d.setP()
	// ruleid: test-value-copies-split
	sink(d.F)
}
