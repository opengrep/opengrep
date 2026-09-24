package p

type T struct{ F string }

func byValue(o T) { o.F = source() }
func byPointer(o *T) { o.F = source() }
func (t T) setV() { t.F = source() }
func (t *T) setP() { t.F = source() }

func caller() {
	a := T{}
	byValue(a)
	// ok: test-value-copies-go
	sink(a.F)
	b := &T{}
	byPointer(b)
	// ruleid: test-value-copies-go
	sink(b.F)
	c := T{}
	c.setV()
	// ok: test-value-copies-go
	sink(c.F)
	d := &T{}
	d.setP()
	// ruleid: test-value-copies-go
	sink(d.F)
}
