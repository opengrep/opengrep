package main

// A named result, which naming does not resolve, read by a closure: the
// closure captures it.
func tainted() (r string) {
	r = source()
	g := func() {
		// ruleid: go_lambda_reads_unresolved_named_result
		sink(r)
	}
	g()
	return
}

func untainted() (r string, err error) {
	r = clean()
	g := func() {
		// ok: go_lambda_reads_unresolved_named_result
		sink(r)
	}
	g()
	return
}
