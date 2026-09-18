package main

func handle(q string) {
	// ok: shadowed-by-local
	sink(q)
}
