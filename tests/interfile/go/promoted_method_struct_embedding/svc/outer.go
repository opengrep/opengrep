package svc

type Outer struct {
	Inner
}

func Run(q string) {
	var o Outer
	o.Query(q)
}
