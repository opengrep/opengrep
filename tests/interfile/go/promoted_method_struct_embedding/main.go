package main

import "example.com/promoted/svc"

func source() string { return "tainted" }

func main() {
	svc.Run(source())
}
