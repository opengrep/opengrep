package svc

type Inner struct{}

func (i Inner) Query(q string) {
	// ruleid: promoted-method-struct-embedding
	sink(q)
}
