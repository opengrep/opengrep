package store

func record(q string) {
	// ok: build-constraint-test-file
	sink(q)
}

func TestWrite() {
	write(source())
}
