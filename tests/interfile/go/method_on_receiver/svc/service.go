package svc

type Service struct{}

func (s *Service) helper(q string) {
	// ruleid: method-on-receiver
	sink(q)
}

func (s *Service) Get(q string) {
	s.helper(q)
}
