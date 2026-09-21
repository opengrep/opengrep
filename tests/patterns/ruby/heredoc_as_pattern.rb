def a(t)
  # ERROR:
  render <<-HTML
hello #{t}
HTML
  render <<-HTML
goodbye #{t}
HTML
  render <<-HTML
hello
HTML
end
