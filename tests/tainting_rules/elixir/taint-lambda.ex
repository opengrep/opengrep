[
  fn
    [:one, y, z], [k, r] ->
      # ruleid: tainted
      sink(z)

    [:two, a, b, 1], [z, g] ->
      # ruleid: tainted
      sink(g)

    {:three, z, r, k}, [] ->
      # ruleid: tainted
      sink(z)

    x ->
      # ruleid: tainted
      sink(x)
  end
  ,

  fn
    [:one, y, z] ->
      # ruleid: tainted
      sink(z)

    [:two, a, b, 1] ->
      # ruleid: tainted
      sink(b)

    {:three, z, r, k} ->
      # ruleid: tainted
      sink(z)

    x ->
      # ruleid: tainted
      sink(x)
  end
  ,

  # ruleid: tainted
  fn x when sink(x) -> x end
]
