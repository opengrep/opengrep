package output

func sink(s string) {
	println(s)
}

func Write(data string) {
	// ruleid: no-sanitiser-test
	sink(data)
}
