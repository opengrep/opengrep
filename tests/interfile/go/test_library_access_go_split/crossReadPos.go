package main

func crossReadPos() {
	m := map[string]string{}
	m["body"] = source()
	// ruleid: test-library-access-taint
	sink(readBody(m))
}
