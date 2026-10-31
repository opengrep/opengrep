object S {
  def predict(modelType: Kind): Vector => Double =
    modelType match {
      case Multinomial =>
        //ERROR:
        features: Vector =>
          sink(features)
      case Gaussian =>
        val scale = 2
        //ERROR:
        features: Vector =>
          sink(features)
    }

  def pairs(rdd: RDD[(Int, Int)]) =
    rdd.foreach {
      //ERROR:
      p: (Int, Int) =>
        sink(p)
    }

  def functions(fs: List[Int => Int]) =
    fs.foreach {
      //ERROR:
      f: (Int => Int) =>
        sink(f)
    }
}
