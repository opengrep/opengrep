package p

type T struct{ F string }

func f(o *T, v string) {
	o.F = v
	o = &T{}
	use(o)
}

func caller() {
	t := &T{}
	f(t, source())
	// ruleid: test-mutate-then-rebind-go
	sink(t.F)
}
