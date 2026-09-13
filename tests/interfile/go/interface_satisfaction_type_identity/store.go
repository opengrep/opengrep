package main

import "example.com/typeident/pkga"

type Store interface {
	Put(s pkga.Service, val string) error
}
