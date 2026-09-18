package main

import "example.com/typealias/svc"

func source() string { return "tainted" }

func main() {
	svc.Run(source())
}
