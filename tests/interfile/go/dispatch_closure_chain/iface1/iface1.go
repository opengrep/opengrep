package iface1

// I1 is the first level of the chain.  Its only impl calls another
// interface's method, forcing a second dispatch-closure iteration.
type I1 interface {
	F(s string) string
}
