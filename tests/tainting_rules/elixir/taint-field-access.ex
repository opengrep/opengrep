defmodule TaintFieldAccess do
  # Field access `x.y` (no parens, no args, no do-block) must propagate
  # taint even when `taint_assume_safe_functions: true`, because it is
  # map/struct field access, not a zero-arity function call.
  def field(upload) do
    # ruleid: taint-field-access
    sink(upload.path)
  end

  # Chained field access.
  def chained(conn) do
    # ruleid: taint-field-access
    sink(conn.assigns.current_user)
  end

  # Zero-arity remote call `x.y()` IS a function call; under
  # `taint_assume_safe_functions: true` the taint is dropped.
  def zero_arity_call(upload) do
    # ok: taint-field-access
    sink(upload.path())
  end

  # Remote call with args — also a call, taint dropped.
  def call_with_args(upload) do
    # ok: taint-field-access
    sink(upload.compute(1, 2))
  end

  # Remote call without parens but with args — still a call.
  def call_no_parens_args(upload) do
    # ok: taint-field-access
    sink(upload.compute 1, 2)
  end

  # Remote call with do-block — a call.
  def call_do_block(upload) do
    # ok: taint-field-access
    sink(upload.with_block do
      :ok
    end)
  end
end
