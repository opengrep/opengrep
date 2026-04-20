package main

// ruleid: go-chan-bidir
func router(ch chan int) {
	ch <- 1
}
