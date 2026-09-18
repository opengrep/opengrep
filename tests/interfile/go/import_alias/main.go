package main

import c "example.com/aliased/internal/httpclient"

func source() string { return "tainted" }

func main() {
	c.Get(source())
}
