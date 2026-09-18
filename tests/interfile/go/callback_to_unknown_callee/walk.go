package main

type Visitor interface {
	Each(s string, f func(string))
}

// A call on a parameter is stored in walk's signature with the shape of
// the lambda, whose body calls walk: the signature refers to itself.
func walk(v Visitor, s string) {
	forward(s)
	v.Each(s, func(t string) { walk(v, t) })
}
