def f()
  i = source()
  #ruleid: test-switch
  sink(i)
  case x
  when 0
    i = sanitize(i)
    #OK:
    sink(i)
  when 1
    unrelated_call()
    #ruleid: test-switch
    sink(i)
  else
    unrelated_call()
    #ruleid: test-switch
    sink(i)
  end
end

def g()
  case source()
  in [[:one, y, z], [k, r]]
    # ruleid: test-switch
    sink(z)

  in [[:two, a, b, 1], [z, g]]
    # ruleid: test-switch
    sink(g)

  in [{three: [z, r, k]}, []]
    # ruleid: test-switch
    sink(z)

  in x
    # ruleid: test-switch
    sink(x)
  end
end
