package helper

type Box struct {
	items []string
}

func NewBox() *Box {
	return &Box{}
}

func (b *Box) Add(v string) {
	b.items = append(b.items, v)
}

func Stash(box *Box, value string) {
	box.Add(value)
}
