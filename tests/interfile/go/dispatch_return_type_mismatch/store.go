package main

// Store.Fetch returns a string.
type Store interface {
	Fetch(q string) string
}
