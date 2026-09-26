package main

type A struct{}

func (a A) Handle(x string) {
	// ok: equal-depth-ambiguous
	sink(x)
}

type B struct{}

func (b B) Handle(x string) {
	// ok: equal-depth-ambiguous
	sink(x)
}

type T struct {
	A
	B
}

func run() {
	t := T{}
	t.Handle(source())
}
