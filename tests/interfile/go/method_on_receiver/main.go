package main

import "example.com/receiver/svc"

func source() string { return "tainted" }

func main() {
	svc.Run(source())
}
