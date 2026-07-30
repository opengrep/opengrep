# Unrelated homonym: same class name and method arity as widget_b.rb,
# but never required by app.rb.  Its presence must not suppress the
# finding through widget_b's Widget#process.
class Widget
  def process(x)
    x.to_s
  end
end
