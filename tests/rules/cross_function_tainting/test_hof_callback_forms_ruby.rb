# Ruby HOF callback forms: each scenario uses its own passthrough,
# its own runner, and its own sink line so findings cannot deduplicate
# across flows.
#
# (1) Lambda arrow [-> (x) { ... }] — works on main; regression check.
# (2) [Proc.new { |x| ... }] — fires after the AST_to_IL rewrite that
#     unwraps Proc.new and lowers the inner Lambda directly.
# (3) [method(:name)] — fires after the resolver reads the variable's
#     [id_svalue] (a [Sym] of the [method(:name)] Call) and looks up
#     [name] via the same path used for bare-name calls.

def source
  "taint"
end

def sink(_x)
end

def apply_cb(cb, v)
  cb.call(v)
end


# ---------- (1) Lambda arrow ----------

def passthrough_arrow(x)
  x
end

def runner_arrow
  cb = ->(x) { passthrough_arrow(x) }
  result = apply_cb(cb, source())
  # ruleid: test-hof-callback-forms-ruby
  sink(result)
end


# ---------- (2) Proc.new ----------

def passthrough_proc(x)
  x
end

def runner_proc
  cb = Proc.new { |x| passthrough_proc(x) }
  result = apply_cb(cb, source())
  # ruleid: test-hof-callback-forms-ruby
  sink(result)
end


# ---------- (3) method(:name) ----------

def passthrough_method(x)
  x
end

def runner_method
  cb = method(:passthrough_method)
  result = apply_cb(cb, source())
  # ruleid: test-hof-callback-forms-ruby
  sink(result)
end


# ---------- Negative: callback returns a constant, no taint to sink ----------

def safe_const(_x)
  "safe"
end

def runner_safe_arrow
  cb = ->(x) { safe_const(x) }
  result = apply_cb(cb, source())
  # ok: test-hof-callback-forms-ruby
  sink(result)
end

def runner_safe_proc
  cb = Proc.new { |x| safe_const(x) }
  result = apply_cb(cb, source())
  # ok: test-hof-callback-forms-ruby
  sink(result)
end

def runner_safe_method
  cb = method(:safe_const)
  result = apply_cb(cb, source())
  # ok: test-hof-callback-forms-ruby
  sink(result)
end
