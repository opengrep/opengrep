object Page {
  // Elements on separate lines are one literal, as in scalac, so the block's
  // value holds every element, not only the last one.
  def header() = {
    val nodes = {
      <meta name="a" />
      <link href={taint_source()}/>
      <script src="b"></script>
    }
    // ruleid: taint-scala-xml-siblings
    render(nodes)
  }

  def clean() = {
    val nodes = {
      <meta name="a" />
      <script src="b"></script>
    }
    // ok: taint-scala-xml-siblings
    render(nodes)
  }
}
