package main

// ruleid: go-chan-out
func producer(out chan<- int) {
	out <- 42
}
