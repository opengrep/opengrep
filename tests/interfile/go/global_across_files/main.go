package main

import "example.com/globals/store"

func readAfterWrite() {
	store.Write()
	// ruleid: global-across-files-go
	sink(store.Read())
}

func readClean() {
	store.WriteClean()
	// ok: global-across-files-go
	sink(store.ReadClean())
}
