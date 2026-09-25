package p

type T struct {
	F string
	G string
}

func declared() {
	var s = T{F: source(), G: "safe"}
	// ok: test-struct-literal-var-go
	sink(s.G)
	// ruleid: test-struct-literal-var-go
	sink(s.F)
}

func declaredWithType() {
	var s T = T{F: source(), G: "safe"}
	// ok: test-struct-literal-var-go
	sink(s.G)
	// ruleid: test-struct-literal-var-go
	sink(s.F)
}
