package svc

type Code uint32

func (c Code) Report(q string) {
	// ruleid: defined-type-with-methods
	sink(q)
}
