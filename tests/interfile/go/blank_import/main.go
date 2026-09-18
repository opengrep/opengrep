package main

import _ "example.com/blank/driver"

type conn struct{}

func (c conn) Get(q string) {
	// ruleid: blank-import
	sink(q)
}

func source() string { return "tainted" }

func main() {
	driver := conn{}
	driver.Get(source())
}
