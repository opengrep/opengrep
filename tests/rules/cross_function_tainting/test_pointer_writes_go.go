package p

type T struct{ F string }

func set(p *string) {
	*p = source()
}

func setf(o *T) {
	o.F = source()
}

func viaCall() {
	s := ""
	set(&s)
	// ruleid: test-pointer-writes-go
	sink(s)
}

func local() {
	s := ""
	p := &s
	*p = source()
	// ruleid: test-pointer-writes-go
	sink(s)
}

func byAddress() {
	t := T{}
	setf(&t)
	// ruleid: test-pointer-writes-go
	sink(t.F)
}

func copied() {
	s := ""
	p := &s
	q := p
	set(q)
	// ruleid: test-pointer-writes-go
	sink(s)
}

func repointed() {
	s := ""
	other := ""
	p := &s
	p = &other
	set(p)
	// ok: test-pointer-writes-go
	sink(s)
}
