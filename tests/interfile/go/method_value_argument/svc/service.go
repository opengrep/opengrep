package svc

type Service struct{}

func (s Service) Get(q string) {
	// ruleid: method-value-argument
	sink(q)
}
