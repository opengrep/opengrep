package main

import "fmt"

type Store struct {
	data string
}

func (s *Store) Send() {
	// ruleid: test-constructor-taint
	sink(s.data)
}

func sink(x string) {
	fmt.Println(x)
}
