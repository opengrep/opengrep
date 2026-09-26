package main

type P struct{}

func (p *P) Run(x string) {
	// ok: field-promotion-by-depth-control
	sink(x)
}

type Q struct{}

func (q *Q) Run(x string) {
	// ruleid: field-promotion-by-depth-control
	sink(x)
}

type C struct {
	X *P
}

type A struct {
	C
}

type B struct {
	X *Q
}

type T struct {
	B
}

func run(t *T) {
	t.X.Run(source())
}
