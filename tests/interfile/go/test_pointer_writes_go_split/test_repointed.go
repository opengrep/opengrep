package main

func testRepointed() {
	s := ""
	other := ""
	p := &s
	p = &other
	set(p)
	// ok: test-pointer-writes-split
	sink(s)
}
