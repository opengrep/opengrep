package store

type DB struct{}

func (d *DB) Query(query string) string { return query }

// This Store carries the sink.  It has the same simple name as beta/store's Store, with
// the same bare class name, method and arity; only the import path
// distinguishes them.
type Store struct{ db *DB }

func (s *Store) Save(query string) string {
	// ruleid: same-name-method-other-package
	return s.db.Query(query)
}
