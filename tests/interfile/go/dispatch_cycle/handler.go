package main

// An interface whose signature must be merged from its concrete impl (Impl),
// which is itself part of a cycle back through the interface.
type Handler interface {
	Handle(x string) string
}

func sink(s string) {}
