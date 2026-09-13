package main

import "example.com/pkgpath/alpha/util"

func source() string { return "tainted" }

func main() {
	util.Run(source())
}
