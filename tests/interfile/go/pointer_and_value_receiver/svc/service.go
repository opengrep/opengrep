package svc

type T struct{}

func (t T) ByValue(q string) {
	// ruleid: pointer-and-value-receiver
	sink(q)
}

func (t *T) ByPointer(q string) {
	// ruleid: pointer-and-value-receiver
	sink(q)
}
