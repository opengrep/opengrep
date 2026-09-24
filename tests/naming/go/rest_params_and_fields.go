package main

type Logger struct {
	ctx []any
}

func newLogger(prefix string, ctx ...any) []any {
	if len(ctx) == 0 {
		ctx = []any{prefix}
	}
	use(ctx)
	return ctx
}
