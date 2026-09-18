package main

func source() string { return "tainted" }

func run(f func()) {
	f()
}

func main() {
	run(func() {
		handle := func(q string) {
			// ok: block-shadow-scoped
			sink(q)
		}
		_ = handle
	})
	handle(source())
}
