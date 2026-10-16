package p

func f(k int, v interface{}) {
	// ERROR: match
	switch k {
	case FOO:
		a()
	}

	// ERROR: match
	switch k {
	case 1, 2:
		a()
	}

	// ERROR: match
	switch v.(type) {
	case int:
		a()
	}

	// ERROR: match
	switch v.(type) {
	case int, string:
		a()
	}
}

func g(k int) {
	// A default branch is not a case label.
	switch k {
	default:
		a()
	}
}
