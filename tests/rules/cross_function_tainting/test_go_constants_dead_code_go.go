package p

func f() error {
	x := source()
	if true {
		return nil
	}
	// ok: test-go-constants-dead-code
	sink(x)
	return nil
}

func g() error {
	x := source()
	if false {
		return nil
	}
	// ruleid: test-go-constants-dead-code
	sink(x)
	return nil
}
