// Tabs on one line and spaces on the next: the indentation of neither
// starts with the other, so the second line ends the indented body.
object A {
	//ERROR:
	def f(x: Int) =
		foo(x)
    def g(x: Int) = 1
	//ERROR:
	def h(x: Int) =
		foo(x)
	def k(x: Int) = 1
}
