#ERROR:
def foo1() do
  42
end

#ERROR:
def foo2(), do: 42

#ERROR:
def foo3 do
  42
end

#ERROR:
def foo4, do: 42

#ERROR:
def foo5() when x > 1 do
  42
end

#ERROR:
def foo6() when x > 1, do: 42

#ERROR:
def foo7 when x > 1 do
  42
end

#ERROR:
def foo8 when x > 1, do: 42
