package main

func main() {
	// p("") is called with an UNtainted argument; the finding depends entirely
	// on p's return signature carrying the source injected inside the cross-file
	// cycle (q -> r(source())), which needs the interfile SCC fixpoint.
	// ruleid: mutual-recursion
	sink(p(""))
}
