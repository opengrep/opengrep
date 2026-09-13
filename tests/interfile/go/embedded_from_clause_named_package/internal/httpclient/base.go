package client

type Base struct{}

func (b Base) Query(q string) {
	// ruleid: embedded-from-clause-named-package
	sink(q)
}
