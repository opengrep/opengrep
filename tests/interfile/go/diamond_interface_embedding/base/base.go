package base

// Base declares M.  Both b.B and c.C embed Base.  In diamond shape,
// projidx's transitive-embedding BFS visits Base via two paths.
type Base interface {
	M(s string) string
}
