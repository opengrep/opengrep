package tracing

import "qualified_bare_name_homonym/codes"

type Span interface {
	SetStatus(code codes.Code, description string)
}

func sink(s string) {}

func source() error { return nil }

// codes.Error is a constant of another package; its bare name is also this
// function's name, and err.Error() is the error interface's method.
func Error(span Span, err error) error {
	span.SetStatus(codes.Error, err.Error())
	return err
}

func use(span Span) {
	err := source()
	// ruleid: qualified-bare-name-homonym
	sink(Error(span, err).Error())
}
