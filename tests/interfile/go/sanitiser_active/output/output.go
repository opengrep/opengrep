package output

func sink(s string) {
	println(s)
}

func Write(data string) {
	// ok: sanitiser-interfile-test
	sink(data)
}
