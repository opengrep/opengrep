package main

import (
	"net/http"

	"example.com/embed-chain/impl"
	"example.com/embed-chain/top"
)

type ConcreteDB struct{}

func (d *ConcreteDB) Query(q string) string { return q }

// handle's static receiver type is top.Top — the chain root.  The
// call t.M(q) must dispatch to impl.Impl.M through four levels of
// transitive embedding inheritance.
func handle(w http.ResponseWriter, r *http.Request, t top.Top) {
	q := r.URL.Query().Get("q")
	_ = t.M(q)
}

func main() {
	var t top.Top = &impl.Impl{DB: &ConcreteDB{}}
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		handle(w, r, t)
	})
	http.ListenAndServe(":8080", nil)
}
