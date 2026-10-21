package p

var Node = 1

func f() {
	type Node struct {
		next *Node
	}
	var n Node
	_ = n
}
