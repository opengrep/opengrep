package main

import "example.com/embedclause/svc"

func source() string { return "tainted" }

func main() {
	svc.Run(source())
}
