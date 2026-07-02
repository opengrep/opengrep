package main

import (
	"net/http"

	"example.com/closure-chain/iface1"
	"example.com/closure-chain/iface2"
	"example.com/closure-chain/impl1"
	"example.com/closure-chain/impl2"
)

type ConcreteDB struct{}

func (d *ConcreteDB) Query(q string) string { return q }

// handle is the source-side call site.  Static type at p.F is the
// interface I1; the actual chain reaches impl2.Impl2.G via two
// successive Dispatch hops (I1 → Impl1, then I2 → Impl2), both of
// which must be resolved during subgraph construction.
func handle(w http.ResponseWriter, r *http.Request, p iface1.I1) {
	q := r.URL.Query().Get("q")
	_ = p.F(q)
}

func main() {
	var next iface2.I2 = &impl2.Impl2{DB: &ConcreteDB{}}
	var p iface1.I1 = &impl1.Impl1{Next: next}
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		handle(w, r, p)
	})
	http.ListenAndServe(":8080", nil)
}
