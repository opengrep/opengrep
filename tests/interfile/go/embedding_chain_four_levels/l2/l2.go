package l2

import "example.com/embed-chain/l1"

// L2 embeds L1 (which embeds base.Base).  For M to be reachable as
// L2.M, the BFS-transitive walk must hop through L1 to Base.
type L2 interface {
	l1.L1
}
