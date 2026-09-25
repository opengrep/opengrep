package store

import "errors"

var G string
var C string

var errMissing = errors.New("missing")

type Writer interface {
	Write()
}

type Tainting struct{}

func (Tainting) Write() {
	G = source()
}

type Clean struct{}

func (Clean) Write() {
	C = "safe"
}

func Read() string {
	return G
}

func ReadClean() string {
	return C
}

type Client interface {
	Authenticate(r string) (string, error)
}

type JWT struct{}

func (JWT) Authenticate(r string) (string, error) {
	sub := source()
	if sub == "" {
		return "", errMissing
	}
	return sub, nil
}

type Anonymous struct{}

func (Anonymous) Authenticate(r string) (string, error) {
	return "anonymous", nil
}
