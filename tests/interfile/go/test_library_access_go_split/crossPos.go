package main

func crossPos() {
	m := map[string]string{}
	writeBody(m, source())
	// ruleid: test-library-access-taint
	sink(m["body"])
}
