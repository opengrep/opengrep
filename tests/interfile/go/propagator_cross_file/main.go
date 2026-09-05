package main

import "example.com/propagator-test/helper"

func source() string {
	return "user-input"
}

func run() {
	box := helper.NewBox()
	helper.Stash(box, source())
	// ruleid: propagator-cross-file-go
	sink(box)
}
