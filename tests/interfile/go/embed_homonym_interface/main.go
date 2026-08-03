package main

import "example.com/embedhomonym/alpha"

// Impl satisfies alpha.Service: it has Write, the only method Service
// requires.  If the embed lift keys on the bare name "Writer" and
// unions beta.Writer's Log into Service's required set, Impl no longer
// satisfies Service, the dispatch edge is lost, and the taint below
// never reaches the sink.
type Impl struct{}

func (i Impl) Write(s string) {
	// ruleid: embed-homonym-interface
	sink(s)
}

func run(svc alpha.Service, data string) {
	svc.Write(data)
}

func main() {
	run(Impl{}, source())
	// ok: embed-homonym-interface
	sink("static")
}
