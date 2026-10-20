# ERROR: match
case k
when foo()
  sink(foo())
end

# ERROR: match
case k
when x.y
  sink(x.y)
end

case k
when foo()
  sink(bar())
end
