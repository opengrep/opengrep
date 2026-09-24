package p

type T struct {
	F string
	G string
}

func (t *T) getG() string {
	return t.G
}

func (t *T) getF() string {
	return t.F
}

func fields() {
	s := &T{F: source(), G: "safe"}
	// ok: test-struct-literal-fields-go
	sink(s.G)
	// ruleid: test-struct-literal-fields-go
	sink(s.F)
}

func methods() {
	s := &T{F: source(), G: "safe"}
	// ok: test-struct-literal-fields-go
	sink(s.getG())
	// ruleid: test-struct-literal-fields-go
	sink(s.getF())
}
