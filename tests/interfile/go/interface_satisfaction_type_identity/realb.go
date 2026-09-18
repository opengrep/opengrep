package main

import "example.com/typeident/pkgb"

type RealB struct{}

func (r *RealB) Put(s pkgb.Service, val string) error {
	// ok: interface-satisfaction-type-identity
	sink(val)
	return nil
}
