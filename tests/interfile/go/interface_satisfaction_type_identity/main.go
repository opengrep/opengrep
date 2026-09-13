package main

import "example.com/typeident/pkga"

func source() string { return "tainted" }

func run(s Store) {
	s.Put(pkga.Service{}, source())
}

func main() {
	run(&RealA{})
}
