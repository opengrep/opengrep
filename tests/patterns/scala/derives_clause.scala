//ERROR:
case class C(x: Int) derives CanEqual

//OK:
case class D(x: Int)

//OK:
case class E(x: Int) derives Show
