package main

import "example.com/importedtype/term"

func source() string { return "tainted" }

func main() {
	var t term.Terminal
	t.Write(source())
}
