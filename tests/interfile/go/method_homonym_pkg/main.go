package main

import (
	"net/http"

	"example.com/collide/alpha/store"
)

func main() {
	s := &store.Store{}
	http.HandleFunc("/x", func(w http.ResponseWriter, r *http.Request) {
		q := r.URL.Query().Get("q")
		// taint flows into alpha's Store.Save (which reaches the sink),
		// NOT beta's homonym Store.Save (which does not).
		_ = s.Save(q)
	})
	http.ListenAndServe(":8080", nil)
}
