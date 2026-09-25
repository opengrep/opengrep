package hof

func Apply(f func() string) string {
	return f()
}

func CallOnly(f func() string) string {
	f()
	return "clean"
}

func Wrap(handler func(c string) string) func(c string) {
	return func(c string) {
		if res := handler(c); res != "" {
			use(res)
		}
	}
}
