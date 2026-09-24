package p

type Q struct{ UID string }

func fill(o *Q) {
	o.UID = source()
}

func helper() {
	x, _ := &Q{}, 1
	fill(x)
}

func handler() {
	x, _ := &Q{}, 1
	helper()
	// ok: test-own-locals-go
	sink(x.UID)
}

func callee() int {
	fill(&Q{})
	return 0
}

func tempCaller() {
	// ok: test-own-locals-go
	sink(&Q{}, callee())
}

func runner(f func()) {
	f()
}

func captured() {
	x := &Q{}
	f := func() {
		fill(x)
	}
	f()
	// ruleid: test-own-locals-go
	sink(x.UID)
}

func capturedThroughHelper() {
	x := &Q{}
	f := func() {
		fill(x)
	}
	runner(f)
	// ruleid: test-own-locals-go
	sink(x.UID)
}
