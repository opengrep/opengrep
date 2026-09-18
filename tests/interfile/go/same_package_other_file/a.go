package main

func source() string { return "tainted" }

func main() {
	helper(source())
}
