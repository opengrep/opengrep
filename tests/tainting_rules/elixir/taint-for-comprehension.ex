def basic(data) do
  for x <- data do
    # ruleid: for_comp_taint
    sink(x)
  end
end

def with_pipe(data) do
  for line <- data do
    # ruleid: for_comp_taint
    line
    |> Base.decode64!()
    |> sink()
  end
end

def multiple_generators(xs, ys) do
  for x <- xs, y <- ys do
    # todoruleid: for_comp_taint
    sink(x)
    # todoruleid: for_comp_taint
    sink(y)
  end
end

def no_taint(data) do
  other = "safe"
  for x <- data do
    # ok: for_comp_taint
    sink(other)
  end
end
