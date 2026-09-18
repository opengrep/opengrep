package main

import "github.com/vendorless/store"

func source() string { return "tainted" }

func main() {
	_ = store.Save(source())
}
