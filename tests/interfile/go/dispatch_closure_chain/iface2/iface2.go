package iface2

// I2 is the second level of the chain.  Its only impl holds the
// sink; only the second dispatch-closure iteration pulls that impl
// into the relevant subgraph.
type I2 interface {
	G(s string) string
}
