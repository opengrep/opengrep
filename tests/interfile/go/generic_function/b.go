package main

func Min[T any](a T, b T) T {
	// ruleid: generic-function
	sink(a)
	return a
}

func Pick[T any](a T) T {
	// ruleid: generic-function
	sink(a)
	return a
}
