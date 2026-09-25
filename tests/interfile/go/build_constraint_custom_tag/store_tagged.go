//go:build integration

package suite

func store(q string) {
	// ruleid: build-constraint-custom-tag
	sink(q)
}
