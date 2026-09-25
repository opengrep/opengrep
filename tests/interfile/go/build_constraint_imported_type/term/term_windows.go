package term

type Terminal struct {
	Handle int
}

func (t Terminal) Write(q string) {
	// ruleid: build-constraint-imported-type
	sink(q)
}

func (t Terminal) Log(q string) {
	// ruleid: build-constraint-imported-type
	sink(q)
}
