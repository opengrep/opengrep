def run(name)
  # ERROR:
  render <<-HTML
hello #{name}
HTML

  render "hello #{name}"
end
