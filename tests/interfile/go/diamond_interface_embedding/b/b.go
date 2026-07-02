package b

import "example.com/diamond/base"

// B is the left arm of the diamond.  Embeds base.Base directly.
type B interface {
	base.Base
}
