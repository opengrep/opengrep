package main

// Store.Put returns error; its second param is a string.
type Store interface {
	Put(id int64, val string) error
}
