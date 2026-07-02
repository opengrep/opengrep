package main

import (
	"net/http"

	"example.com/collide/alpha/store"
)

type DB struct{}

func (d *DB) Query(query string) string { return query }

func main() {
	db := &DB{}
	http.HandleFunc("/x", func(w http.ResponseWriter, r *http.Request) {
		q := r.URL.Query().Get("q")
		_ = store.Save(db, q)
	})
	http.ListenAndServe(":8080", nil)
}
