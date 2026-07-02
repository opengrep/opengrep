package main

func main() {
	taintedInput := getTaintedData()
	result := processData(taintedInput)

	// Test anonymous function
	x := getTainted()
	a := passThrough(x)
	// ruleid: simple_go_taint
	sink(a)

	return
}