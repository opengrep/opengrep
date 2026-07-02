package c

import "example.com/diamond/base"

// C is the right arm of the diamond.  Embeds base.Base directly.
type C interface {
	base.Base
}
