package main

type C struct{}

func (c C) Handle(x string) {
	// ok: promotion-by-depth
	sink(x)
}

type A struct {
	C
}

type B struct{}

func (b B) Handle(x string) {
	// ruleid: promotion-by-depth
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
