package p

type Inner struct{ S string }

type Req struct {
	H  map[string]string
	In Inner
}

func (r Req) SetHeader(v string) { r.H["k"] = v }

func (r Req) SetInner(v string) { r.In.S = v }

func caller() {
	a := Req{H: map[string]string{}}
	a.SetHeader(source())
	// ruleid: test-value-copies-shared-data-go
	sink(a.H)
	b := Req{H: map[string]string{}}
	b.SetInner(source())
	// ok: test-value-copies-shared-data-go
	sink(b.In.S)
}
