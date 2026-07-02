package main




// Test anonymous function taint flow
var getTainted = func() string {
	y := source()
	return y
}

var passThrough = func(z string) string {
	w := z
	return w
}

