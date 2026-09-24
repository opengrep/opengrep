package p

func rebind(ctx string, x string) {
	ctx = x
	use(ctx)
}

func caller() {
	c := "safe"
	rebind(c, source())
	// ok: test-param-rebinding-go
	sink(c)
}
