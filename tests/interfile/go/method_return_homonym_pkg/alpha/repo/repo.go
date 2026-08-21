package repo

type DB struct{}

func (d *DB) Query(query string) string { return query }

type Inner struct{ db *DB }

func (i *Inner) Save(query string) string {
	// ruleid: method-return-homonym-pkg
	return i.db.Query(query)
}

type Repo struct{}

func (r *Repo) Get() *Inner { return &Inner{} }
