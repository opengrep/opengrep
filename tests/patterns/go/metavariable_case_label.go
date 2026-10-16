package p

func f(k int, v interface{}) {
	// ERROR: match
	switch k {
	case FOO:
		a()
	}

	// ERROR: match
	switch k {
	case 2:
		a()
	}

	// ERROR: match
	switch v.(type) {
	case int:
		a()
	}
}
