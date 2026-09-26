package main

type C struct{}

func (c C) Handle(x string) {
	// ok: promotion-by-depth-control
	sink(x)
}

type A struct {
	C
}

type B struct{}

func (b B) Handle(x string) {
	// ruleid: promotion-by-depth-control
	sink(x)
}

type T struct {
	B
}

func run() {
	t := T{}
	t.Handle(source())
}
