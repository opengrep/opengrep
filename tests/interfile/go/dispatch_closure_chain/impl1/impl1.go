package impl1

import "example.com/closure-chain/iface2"

// Impl1 is the only implementation of iface1.I1.  Its F passes the
// argument forward into iface2.I2.G — a CALL edge to I2.G, NOT a
// Dispatch edge.  The reverse-callee BFS will follow this Call edge
// and bring I2.G into the subgraph after the first dispatch-closure
// iteration; but the impl of I2.G needs a SECOND dispatch-closure
// iteration to land in the subgraph.
type Impl1 struct {
	Next iface2.I2
}

func (i *Impl1) F(s string) string {
	return i.Next.G(s)
}
