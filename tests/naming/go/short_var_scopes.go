package main

var (
	ga = 1
	gb = 2
)

func f(ctx int) error {
	ctx, span := start(ctx)
	use(span, ctx)
	a, err := g()
	if err != nil {
		return err
	}
	if err := h(); err != nil {
		return err
	}
	b, err := k()
	if err != nil {
		use(b)
	}
	use(a, ga, gb)
L:
	var e, e2 = 5, 6
	use(e, e2)
	goto L
	return err
}
