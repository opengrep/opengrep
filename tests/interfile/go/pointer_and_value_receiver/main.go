package main

import "example.com/receivers/svc"

func source() string { return "tainted" }

func main() {
	svc.Run(source())
}
