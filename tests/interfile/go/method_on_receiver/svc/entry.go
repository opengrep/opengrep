package svc

func Run(q string) {
	s := &Service{}
	s.Get(q)
}
