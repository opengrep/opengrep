package svc

type Store struct{}

func (s Store) Get(q string) {
	// ruleid: type-alias
	sink(q)
}
