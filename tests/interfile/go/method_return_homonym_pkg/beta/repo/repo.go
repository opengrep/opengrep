package repo

// Homonym Inner/Repo in a different package, no sink.
type Inner struct{}

func (i *Inner) Save(query string) string { return "noop: " + query }

type Repo struct{}

func (r *Repo) Get() *Inner { return &Inner{} }
