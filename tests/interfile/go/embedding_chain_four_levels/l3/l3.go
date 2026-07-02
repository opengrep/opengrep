package l3

import "example.com/embed-chain/l2"

// L3 embeds L2 (which embeds L1 which embeds Base).  Three hops to
// the leaf method declaration.
type L3 interface {
	l2.L2
}
