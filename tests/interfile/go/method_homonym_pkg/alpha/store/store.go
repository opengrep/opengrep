package store

type DB struct{}

func (d *DB) Query(query string) string { return query }

// The SINK-bearing Store.  Homonym with beta/store's Store (same leaf
// class name + method + arity); only the import path distinguishes them.
type Store struct{ db *DB }

func (s *Store) Save(query string) string {
	// ruleid: method-homonym-pkg
	return s.db.Query(query)
}
