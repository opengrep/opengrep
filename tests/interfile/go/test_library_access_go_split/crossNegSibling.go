package main

func crossNegSibling() {
	m := map[string]string{}
	m["body"] = "safe"
	writeUser(m, source())
	// ok: test-library-access-taint
	sink(m["body"])
}
