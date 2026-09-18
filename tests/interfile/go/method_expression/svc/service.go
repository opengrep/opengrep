package svc

type Service struct{}

func (s Service) Read() string {
	return source()
}
