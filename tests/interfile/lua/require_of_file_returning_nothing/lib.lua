local M = {}

function M.leak(v)
  -- ok: require_of_file_returning_nothing
  sink(v)
end
