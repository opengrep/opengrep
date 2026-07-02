package main

import (
	"example.com/sanitiser-test/middle"
	"example.com/sanitiser-test/output"
)

func source() string {
	return "user-input"
}

func handler() {
	x := source()
	y := middle.Process(x)
	output.Write(y)
}

func main() {
	handler()
}
