package main

import "example.com/importedtype/term"

func console() {
	var t term.Terminal
	t.Log(source())
}
