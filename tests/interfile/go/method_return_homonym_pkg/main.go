package main

import (
	"net/http"

	"example.com/collide/alpha/repo"
)

func main() {
	r := &repo.Repo{}
	http.HandleFunc("/x", func(w http.ResponseWriter, req *http.Request) {
		q := req.URL.Query().Get("q")
		// r.Get() returns alpha's *Inner, whose Save reaches db.Query (sink).
		// Requires resolving Repo.Get()'s return type to alpha's Inner, not
		// beta's homonym Inner.
		_ = r.Get().Save(q)
	})
	http.ListenAndServe(":8080", nil)
}
