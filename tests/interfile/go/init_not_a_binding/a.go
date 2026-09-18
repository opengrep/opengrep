package main

func init() {
	_ = 1
}

func setup(q string) {
	// ruleid: init-not-a-binding
	sink(q)
}
