object XmlLiterals {
  def page(appId: String, rows: Seq[String]) = {
    val msg = <div class="row">Application {appId} not found.</div>
    val content =
      <div class="container-fluid">
        <!-- a comment -->
        <span id={s"thread_${appId}_tr"} type='text/css' data-bs-toggle="collapse"/>
        <![CDATA[ x < y ]]>
        &nbsp;{{literal braces}}
        <ul>{rows.map(r => <li>{r}</li>)}</ul>
      </div>
    val nodes = <a/><b/>
    msg ++ content ++ nodes
  }

  val text = <integer_value int="Ten">Ten</integer_value>.toString

  def comparisons(a: Int, b: Int) = a < b && a<b && (a <= b)

  def unspacedComparisons(a: Int, b: Int, xs: List[Int]) =
    if (a <b) xs.filter(_ <b) else xs
}
