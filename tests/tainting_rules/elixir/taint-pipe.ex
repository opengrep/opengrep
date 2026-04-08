def single_pipe(data) do
  # ruleid: pipe_taint
  data |> sink()
end

def chain(data) do
  # ruleid: pipe_taint
  data
  |> decode()
  |> sink()
end

def pipe_with_args(data) do
  # ruleid: pipe_taint
  data |> sink(:option)
end

def no_taint(data) do
  other = "safe"
  # ok: pipe_taint
  other |> sink()
end
