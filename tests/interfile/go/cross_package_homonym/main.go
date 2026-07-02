package main

import (
	"net/http"

	"example.com/homonym-check/pkga"
)

type DB struct{}

func (d *DB) Query(query string) string { return query }

func main() {
	db := &DB{}
	http.HandleFunc("/x", func(w http.ResponseWriter, r *http.Request) {
		pkga.Handle(w, r, db)
	})
	http.ListenAndServe(":8080", nil)
}
