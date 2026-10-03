package p

// A bare name labels the case (a constant): it is compared with the
// scrutinee and binds nothing, so the later cases stay reachable.
func bareNameCaseKeepsLaterCases(mode int) {
	q := ""
	switch mode {
	case A:
		q = "safe"
	case B:
		q = taint_source()
		// ruleid: taint-switch-case-go
		sink(q)
	}
}

func bareNameCaseIsNotABindingTarget() {
	y := taint_source()
	switch y {
	case RED:
	}
	// ok: taint-switch-case-go
	sink(RED)
}
