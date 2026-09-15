package store

type Database interface {
	Query(query string) string
}

// Free function (not a method).  This is the SINK-bearing Save.
func Save(db Database, query string) string {
	// ruleid: same-name-free-function-other-package
	return db.Query(query)
}
