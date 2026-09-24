package main

import "example.com/cb/hof"

func tainted() string {
	return source()
}

func taintedHandler(c string) string {
	return source()
}

func test() {
	// ruleid: callback-result-across-files-go
	sink(hof.Apply(tainted))
	// ok: callback-result-across-files-go
	sink(hof.CallOnly(tainted))
	// ok: callback-result-across-files-go
	sink(hof.Wrap(taintedHandler))
}
