def run(name)
  # ERROR:
  render <<-HTML
hello #{name}
HTML

  # ERROR:
  render "hello #{name}"

  render "hello"
end
