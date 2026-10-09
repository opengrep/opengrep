package main

type Store struct{ q, r string }

// A closure reading a field of the method's receiver: inside the closure the
// receiver is the method's.
func (s *Store) Get(ctx int) {
	s.withSession(func(sess int) {
		// ruleid: lambda-sig-go-captured-receiver
		sink(s.q + "x")
		// ok: lambda-sig-go-captured-receiver
		sink(s.r + "x")
	})
}

func (s *Store) withSession(f func(int)) { f(0) }

func caller() {
	s := &Store{}
	s.q = source()
	s.Get(0)
}
