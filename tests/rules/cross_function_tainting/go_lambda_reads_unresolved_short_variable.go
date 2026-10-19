package main

// The variable of a type switch guard, which naming does not resolve, read by
// a closure: the closure captures it.
func typeSwitchGuard() {
	switch v := source().(type) {
	case string:
		g := func() {
			// ruleid: go_lambda_reads_unresolved_short_variable
			sink(v)
		}
		g()
	}
}

// The variable of a select case, which naming does not resolve, read by a
// closure: the closure captures it.
func selectCase() {
	select {
	case m := <-source():
		g := func() {
			// ruleid: go_lambda_reads_unresolved_short_variable
			sink(m)
		}
		g()
	}
}

func untaintedTypeSwitchGuard() {
	switch v := clean().(type) {
	case string:
		g := func() {
			// ok: go_lambda_reads_unresolved_short_variable
			sink(v)
		}
		g()
	}
}
