package main

func source() string { return "tainted" }

func main() {
	handle := func(q string) {
		// ruleid: shadowed-by-local
		sink(q)
	}
	handle(source())
}
