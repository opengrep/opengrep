package main

func source() string { return "tainted" }

func run(s Store) {
	s.Fetch(source())
}

func main() {
	run(&PGStore{})
}
