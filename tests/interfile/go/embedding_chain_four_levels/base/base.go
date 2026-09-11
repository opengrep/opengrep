package base

// Base — chain leaf.  M is declared only here and must propagate up
// the embedding chain to Top via four BFS hops.
type Base interface {
	M(s string) string
}
