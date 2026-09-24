package main

import "example.com/dispatch/store"

func writeThroughInterface(w store.Writer) {
	w.Write()
	// ruleid: global-through-interface-go
	sink(store.Read())
}

func cleanGlobal(w store.Writer) {
	w.Write()
	// ok: global-through-interface-go
	sink(store.ReadClean())
}

func returnNextToErrorGlobal(c store.Client, r string) {
	id, _ := c.Authenticate(r)
	// ruleid: global-through-interface-go
	sink(id)
}
