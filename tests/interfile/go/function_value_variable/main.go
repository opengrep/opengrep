package main

func source() string { return "tainted" }

func main() {
	h := Handle
	h(source())
}
