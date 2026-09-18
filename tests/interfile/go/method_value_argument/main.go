package main

import "example.com/methodvalue/svc"

func source() string { return "tainted" }

func main() {
	svc.Run(source())
}
