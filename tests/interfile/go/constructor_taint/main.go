package main

import "os"

func source() string {
	return os.Getenv("SECRET")
}

func main() {
	tainted := source()
	s := Store{data: tainted}
	s.Send()
}
