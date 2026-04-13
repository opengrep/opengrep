def foo() do
  val x = source()
  case foo() do
    #ruleid: taint
    ^x -> sink(x)
  end
end
