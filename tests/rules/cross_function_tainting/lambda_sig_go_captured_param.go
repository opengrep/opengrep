package main

type Store struct{}

// A closure capturing a parameter of a method: the receiver takes no
// parameter index, so the captured parameter is the method's second.
func (s *Store) Get(ctx int, query string) {
	s.withSession(func(sess int) {
		// ruleid: lambda-sig-go-captured-param
		sink(query)
		// ok: lambda-sig-go-captured-param
		sink(ctx)
	})
}

func (s *Store) withSession(f func(int)) { f(0) }

func caller(s *Store) { s.Get(0, source()) }
