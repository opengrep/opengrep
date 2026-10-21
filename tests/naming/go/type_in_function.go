package p

var T = 1

func f() int {
	type T int
	return int(T(2))
}

func g() int {
	return T
}
