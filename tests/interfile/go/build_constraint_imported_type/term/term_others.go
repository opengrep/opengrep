//go:build !windows

package term

type Terminal struct {
	Fd int
}

func (t Terminal) Write(q string) {
	// ruleid: build-constraint-imported-type
	sink(q)
}

func (t Terminal) Log(q string) {
	// ok: build-constraint-imported-type
	sink(q)
}
