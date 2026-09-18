require_relative 'inner'

# `include Inner` appears in a MODULE body, not a class body.  If
# projidx's class_body_extra_parents hook only fires inside `class`
# blocks, this edge is lost — Outer's parents are [] in
# Type_state.parent_class, and any class that includes Outer never
# gets Inner's methods.
module Outer
  include Inner
end
