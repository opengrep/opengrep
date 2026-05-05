// Cross-function field-sensitive taint through Go's map library
// idioms. Maps flow across function boundaries as parameters and
// returns, and field-sensitivity must survive the call.

package main

func writeBody(m map[string]string, v string) {
	m["body"] = v
}

func writeUser(m map[string]string, v string) {
	m["user"] = v
}

func readBody(m map[string]string) string {
	return m["body"]
}

func crossPos() {
	m := map[string]string{}
	writeBody(m, source())
	// ruleid: test-library-access-taint
	sink(m["body"])
}

func crossNegSibling() {
	m := map[string]string{}
	m["body"] = "safe"
	writeUser(m, source())
	// ok: test-library-access-taint
	sink(m["body"])
}

func crossReadPos() {
	m := map[string]string{}
	m["body"] = source()
	// ruleid: test-library-access-taint
	sink(readBody(m))
}
