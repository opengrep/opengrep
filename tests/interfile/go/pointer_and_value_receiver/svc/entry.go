package svc

func Run(q string) {
	t := &T{}
	t.ByValue(q)
	t.ByPointer(q)
}
