package main

import (
	"net/http"

	"example.com/dispatch-closure/iface"
	"example.com/dispatch-closure/impl"
)

// ConcreteDB satisfies impl.DB; its Query returns its input.  The
// pattern-sink in rule.yaml matches `$DB.Query($QUERY)` so the call
// `p.DB.Query(s)` inside impl.ProcessorImpl.Process is the sink.
type ConcreteDB struct{}

func (d *ConcreteDB) Query(q string) string { return q }

// handle is the source-side call site.  The static type of `p` is
// iface.Processor (an interface), so the call `p.Process(q)` becomes
// a Call edge to iface.Processor.Process — not to the concrete
// impl.ProcessorImpl.Process.
func handle(w http.ResponseWriter, r *http.Request, p iface.Processor) {
	// SOURCE: user-controlled URL query param.
	q := r.URL.Query().Get("q")
	_ = p.Process(q)
}

func main() {
	var p iface.Processor = &impl.ProcessorImpl{DB: &ConcreteDB{}}
	http.HandleFunc("/x", func(w http.ResponseWriter, r *http.Request) {
		handle(w, r, p)
	})
	http.ListenAndServe(":8080", nil)
}
