package main

import "example.com/clause/internal/httpclient"

func source() string { return "tainted" }

func main() {
	client.Get(source())
}
