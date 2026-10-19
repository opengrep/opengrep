package p

type Error struct{}

func (e Error) Error() string {
	return ""
}

func f(x Error) {}
