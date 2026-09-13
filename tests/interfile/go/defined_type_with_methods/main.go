package main

import "example.com/definedtype/svc"

func source() string { return "tainted" }

func main() {
	svc.Run(source())
}
