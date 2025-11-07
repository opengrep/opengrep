package main

func getTaintedData() string {
	return source()
}

func processData(data string) string {
	// ruleid: simple_go_taint
	return sink(data)
}

// Test anonymous function taint flow
var getTainted = func() string {
	y := source()
	return y
}

var passThrough = func(z string) string {
	w := z
	return w
}

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