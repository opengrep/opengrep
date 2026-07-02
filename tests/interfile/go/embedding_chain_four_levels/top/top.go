package top

import "example.com/embed-chain/l3"

// Top — chain root.  Four embedding hops separate this interface
// from the M declaration on base.Base.  The dispatch edge from
// Top.M to Impl.M is only present if the BFS-transitive embedding
// walk reaches Base from Top.
type Top interface {
	l3.L3
}
