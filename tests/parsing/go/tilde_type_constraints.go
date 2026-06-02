package main

// tilde in a single type parameter
func SingleTilde[S ~uint8]() {}

// tilde union in a type parameter
func TildeUnion[S ~uint8 | ~string]() {}

// multiple type parameters, one with tilde
func MultiParam[S ~uint8, P interface{ Foo() }]() {}

// tilde with qualified type
func QualifiedTilde[S ~io.Reader]() {}

// tilde with multiple names sharing a constraint
func SharedConstraint[S, T ~int]() {}

// type definition with tilde constraint
type Stack[T ~[]E, E any] struct{}
