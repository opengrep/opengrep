package alpha

// Writer declares exactly one method.
type Writer interface {
	Write(s string)
}

// Service embeds alpha.Writer, so its required method set is {Write}.
// An unrelated same-named interface in another package must not
// contribute its methods here.
type Service interface {
	Writer
}
