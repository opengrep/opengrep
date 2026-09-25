//go:build !integration

package suite

func store(q string) {
	// ok: build-constraint-custom-tag
	sink(q)
}
