package store

func source() string { return "tainted" }

func Save() {
	record(source())
}

func write(q string) {
	// ruleid: build-constraint-test-file
	sink(q)
}
