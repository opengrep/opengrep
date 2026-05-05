package main

func p(x interface{}) interface{} { return q(x) }
func q(x interface{}) interface{} { return r(source()) }
func r(x interface{}) interface{} {
	if cond() {
		return p(x)
	}
	return x
}
func test_3cycle() {
	// ruleid: test-recursion-fixpoint
	sink(p(0))
}
