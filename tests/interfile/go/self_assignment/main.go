package main

// A variable assigned from an expression over itself: the symbolic value
// of its later uses is the right-hand side, whose payload holds this very
// occurrence.
func collect(err error) []error {
	var errs []error
	errs = append(errs, err)
	return errs
}

func main() {
	data := source()
	forward(data)
}
