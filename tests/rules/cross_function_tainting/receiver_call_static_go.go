// A call on a method's receiver reaches the method of the receiver's
// declared type; Go has no overriding, so another type's method is not reached.
package main

type A struct{}

func (a *A) Run(x string) {
	a.m(x)
}

func (a *A) m(x string) {
	// ruleid: receiver_call_static_go
	sink(x)
}

type B struct{ A }

func (b *B) m(x string) {
	// ok: receiver_call_static_go
	sink(x)
}

type C struct{}

func (c *C) m(x string) {
	// ok: receiver_call_static_go
	sink(x)
}

func main() {
	b := &B{}
	b.Run(source())
}
