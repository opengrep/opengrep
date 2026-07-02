package top

import (
	"example.com/diamond/b"
	"example.com/diamond/c"
)

// Top closes the diamond: it embeds both arms.  Transitively, Top
// must inherit M (declared only on base.Base) — and exactly once,
// not twice.
type Top interface {
	b.B
	c.C
}
