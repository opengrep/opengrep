package main

type T struct{ F string }

func set(p *string) {
	*p = source()
}

func setf(o *T) {
	o.F = source()
}
