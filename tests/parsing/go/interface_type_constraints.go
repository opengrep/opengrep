package main

import "io"

// pointer type in a type set
type PtrConstraint interface {
	*int
}

// qualified type in a type set
type QualConstraint interface {
	io.Reader
}

// union of qualified types
type UnionQual interface {
	io.Reader | io.Writer
}

// union mixing pointer and qualified
type UnionPtrQual interface {
	*int | io.Reader
}

// tilde with qualified type
type TildeQual interface {
	~io.Reader
}

// multi-term union with pointer, qualified, and underlying
type Mixed interface {
	*int | io.Reader | ~string
}

// embedded alongside a method
type WithMethod interface {
	Method() error
	*int | io.Reader
}
