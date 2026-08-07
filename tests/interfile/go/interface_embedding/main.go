package main

import (
	"net/http"

	"example.com/embedding-check/client"
	"example.com/embedding-check/impl"
)

type ConcreteDB struct{}

func (d *ConcreteDB) Query(query string) string { return query }

func handle(w http.ResponseWriter, r *http.Request, c client.Client) {
	// SOURCE: user-controlled URL query param.
	token := r.URL.Query().Get("token")
	// c is the Client interface.  Authenticate is inherited via
	// embedding from auth.Authenticator.  Without embedding support,
	// Type_state.methods[Client] lacks Authenticate and the dispatch
	// edge to impl.ClientImpl.Authenticate is missing.
	_ = c.Authenticate(token)
}

func main() {
	c := &impl.ClientImpl{DB: &ConcreteDB{}}
	http.HandleFunc("/login", func(w http.ResponseWriter, r *http.Request) {
		handle(w, r, c)
	})
	http.ListenAndServe(":8080", nil)
}
