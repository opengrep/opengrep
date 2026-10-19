object S {
  def run(rdd: RDD[Vector]) = {
    //ERROR:
    rdd.foreach {
      vector: Vector =>
        sink(vector)
    }
  }
}
