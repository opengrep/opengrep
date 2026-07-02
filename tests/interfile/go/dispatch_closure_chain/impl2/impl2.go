package impl2

// DB is the sink-bearing back-end.
type DB interface {
	Query(q string) string
}

// Impl2 is the only implementation of iface2.I2.  This is the
// vertex the dispatch-closure must reach via a SECOND iteration —
// after iteration 1 has placed Impl1.F in the subgraph (which then
// drags I2.G in via the reverse-callee BFS).
type Impl2 struct {
	DB DB
}

func (i *Impl2) G(s string) string {
	// ruleid: dispatch-closure-chain
	return i.DB.Query(s)
}
