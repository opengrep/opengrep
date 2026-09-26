package p

func mk(v string) func() string {
	var x string
	x = v
	return func() string { return x }
}

func otherCallIsClean() {
	a := mk(source())
	b := mk("clean")
	_ = a
	// ok: test-escaped-local-per-call-go
	sink(b())
}

func sameCallIsTainted() {
	b := mk(source())
	// ruleid: test-escaped-local-per-call-go
	sink(b())
}
