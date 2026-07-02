package main

import (
	"net/http"

	"example.com/diamond/impl"
	"example.com/diamond/top"
)

type ConcreteDB struct{}

func (d *ConcreteDB) Query(q string) string { return q }

// handle receives a top.Top — the diamond root.  Static call goes
// to top.Top.M; dispatch must route via the BFS-inherited M down to
// impl.Impl.M, where the sink lives.  If diamond inheritance fails
// to bring M into Type_state.methods[top.Top], the call site can't
// resolve to an FBDecl and the dispatch chain breaks.
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
