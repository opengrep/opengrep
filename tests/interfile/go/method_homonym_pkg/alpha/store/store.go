package store

type DB struct{}

func (d *DB) Query(query string) string { return query }

// This Store carries the sink.  It is a homonym of beta/store's Store, with
// the same bare class name, method and arity; only the import path
// distinguishes them.
type Store struct{ db *DB }

func (s *Store) Save(query string) string {
	// ruleid: method-homonym-pkg
	return s.db.Query(query)
}
