package main

import "example.com/conversion/svc"

func source() string { return "tainted" }

func main() {
	_ = svc.Wrapper(source())
	svc.Run(source())
}
