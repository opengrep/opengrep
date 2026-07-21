package main

func source() string { return "tainted" }

func run(s Store) {
	s.Put(1, source())
}

func main() {
	run(&RealStore{})
}
