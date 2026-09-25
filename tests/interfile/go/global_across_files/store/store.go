package store

var G string
var C string

func Write() {
	G = source()
}

func WriteClean() {
	C = "safe"
}

func Read() string {
	return G
}

func ReadClean() string {
	return C
}
