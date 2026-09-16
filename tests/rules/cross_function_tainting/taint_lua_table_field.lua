local M = {}
M.data = source()
-- ruleid: taint-lua-table-field
sink(M.data)
