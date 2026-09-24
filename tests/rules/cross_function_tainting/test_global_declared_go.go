package main

var G string
var C string

func writeTainted() {
	G = source()
}

func writeClean() {
	C = "safe"
}

func readAfterWrite() {
	writeTainted()
	// ruleid: test-global-declared-go
	sink(G)
}

func readClean() {
	writeClean()
	// ok: test-global-declared-go
	sink(C)
}

func readShadowed() {
	G := "safe"
	writeTainted()
	// ok: test-global-declared-go
	sink(G)
}
