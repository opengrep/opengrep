package svc

type Alias = Store

func Run(q string) {
	var a Alias
	a.Get(q)
}
