package main

func source() string { return "tainted" }

func main() {
	setup(source())
}
