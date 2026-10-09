package p

func tupleCaptured(db *DB) error {
	q, _ := source()
	return db.WithSession(func(s *Session) error {
		// ruleid: go_tuple_binding_captured
		return sink("SELECT " + q)
	})
}

func tupleCapturedSafe(db *DB) error {
	q, _ := safe()
	return db.WithSession(func(s *Session) error {
		// ok: go_tuple_binding_captured
		return sink("SELECT " + q)
	})
}
