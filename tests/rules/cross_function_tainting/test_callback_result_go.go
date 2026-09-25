package main

func apply(f func() string) string {
	return f()
}

func callOnly(f func() string) string {
	f()
	return "clean"
}

func sinkIt(f func() string) {
	// ruleid: test-callback-result-go
	sink(f())
}

type Box struct {
	V string
}

func store(b *Box, f func() string) {
	b.V = f()
}

func wrap(handler func(c string) string) func(c string) {
	return func(c string) {
		if res := handler(c); res != "" {
			use(res)
		}
	}
}

type Opts struct {
	Cb func(s string)
}

func run(o Opts, x string) {
	o.Cb(x)
}

func getCb(o *Opts) func(s string) {
	return o.Cb
}

func tainted() string {
	return source()
}

func taintedHandler(c string) string {
	return source()
}

func test() {
	// ruleid: test-callback-result-go
	sink(apply(tainted))
	// ok: test-callback-result-go
	sink(callOnly(tainted))
	sinkIt(tainted)
	b := &Box{}
	store(b, tainted)
	// ruleid: test-callback-result-go
	sink(b.V)
	// ok: test-callback-result-go
	sink(wrap(taintedHandler))
}

func fieldCallbacks() {
	run(Opts{Cb: func(s string) {
		// ruleid: test-callback-result-go
		sink(s)
	}}, source())
	f := getCb(&Opts{Cb: func(s string) {
		// ruleid: test-callback-result-go
		sink(s)
	}})
	f(source())
}

func twice(f func(s string) string, a string, b string) {
	// ok: test-callback-result-go
	sink(f(a))
	// ruleid: test-callback-result-go
	sink(f(b))
}

func identity(s string) string {
	return s
}

func perCall() {
	twice(identity, "clean", source())
}

type User struct {
	Name  string
	Token string
}

func name(f func() User) string {
	return f().Name
}

func token(f func() User) string {
	return f().Token
}

func makeUser() User {
	return User{Name: "x", Token: source()}
}

func perField() {
	// ok: test-callback-result-go
	sink(name(makeUser))
	// ruleid: test-callback-result-go
	sink(token(makeUser))
}
