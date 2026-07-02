package l1

import "example.com/embed-chain/base"

// L1 embeds base.Base.  Embedded methods are parsed as Spread
// expressions in the interface body — the project-wide embeds map
// must track this edge so M propagates up.
type L1 interface {
	base.Base
}
