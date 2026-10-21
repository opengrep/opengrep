package p

type Stringer interface {
	String() string
}

func f() string {
	return String()
}
