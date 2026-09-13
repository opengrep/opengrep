package main

func source() string { return "tainted" }

func main() {
	q := source()
	_ = Min(q, "x")
	_ = Min[string](q, "y")
	h := Pick[string]
	_ = h(q)
}
