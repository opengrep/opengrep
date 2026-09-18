package main

import . "example.com/dotimport/util"

func source() string { return "tainted" }

func main() {
	Run(source())
}
