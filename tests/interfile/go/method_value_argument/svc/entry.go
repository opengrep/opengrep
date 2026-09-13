package svc

func register(q string, h func(string)) {
	h(q)
}

func Run(q string) {
	var s Service
	register(q, s.Get)
}
