package main

import "example.com/methodexpr/svc"

func main() {
	// ruleid: method-expression
	sink(svc.Run())
}
