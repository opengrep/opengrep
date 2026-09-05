package tracing

import "qualified_leaf_homonym/codes"

type Span interface {
	SetStatus(code codes.Code, description string)
}

func sink(s string) {}

func source() error { return nil }

// codes.Error is a constant of another package; its leaf is also this
// function's name, and err.Error() is the error interface's method.
func Error(span Span, err error) error {
	span.SetStatus(codes.Error, err.Error())
	return err
}

func use(span Span) {
	err := source()
	// ruleid: qualified-leaf-homonym
	sink(Error(span, err).Error())
}
