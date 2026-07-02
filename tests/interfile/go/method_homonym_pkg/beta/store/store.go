package store

// Homonym Store in a DIFFERENT package (same basename "store"), same
// method + arity, no sink.  Only the import path tells it apart from
// alpha/store's Store.
type Store struct{}

func (s *Store) Save(query string) string {
	return "noop: " + query
}
