package fixtures

import "sync/atomic"

type Value[T any] interface {
	Get() T
}

type WithEmbeddedInterface struct {
	Value[int]
	Enabled bool
	Code    int
}

type WithEmbeddedPointer struct {
	*atomic.Pointer[string]
	Enabled bool
}

type WithArrayField struct {
	Values  [2]int
	Enabled bool
}
