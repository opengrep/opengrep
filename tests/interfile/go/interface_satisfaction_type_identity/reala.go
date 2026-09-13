package main

import "example.com/typeident/pkga"

type RealA struct{}

func (r *RealA) Put(s pkga.Service, val string) error {
	// ruleid: interface-satisfaction-type-identity
	sink(val)
	return nil
}
