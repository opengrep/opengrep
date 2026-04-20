package main

// ruleid: go-chan-in
func consumer(in <-chan int) int {
	return <-in
}
