package main

func literalInVariable() {
	u := source()
	f := func() string { return u }
	// ruleid: test-closure-captured-go
	sink(f())
}

func with(cb func() string) string {
	return cb()
}

func retry(f func() string) string {
	return f()
}

func wrapped(cb func() string) func() string {
	return func() string {
		return cb()
	}
}

func callbackThroughWrapper(u string) string {
	return retry(wrapped(func() string {
		return u
	}))
}

func mkLocal() func() string {
	u := source()
	return func() string { return u }
}

func returnedOverLocal() {
	f := mkLocal()
	// ruleid: test-closure-captured-go
	sink(f())
}

func main() {
	// ruleid: test-closure-captured-go
	sink(callbackThroughWrapper(source()))
	// ruleid: test-closure-captured-go
	sink(with(func() string { return source() }))
}
