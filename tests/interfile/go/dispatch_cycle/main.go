package main

func source() string {
	return ""
}

func main() {
	var h Handler = Impl{}
	// h.Handle is called with an UNtainted argument; the finding depends on the
	// dispatched Impl.Handle return signature carrying the source injected
	// inside the cycle, which needs the SCC fixpoint to converge.
	// ruleid: dispatch-cycle
	sink(h.Handle(""))
}
