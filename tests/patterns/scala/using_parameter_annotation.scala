class A(using @constructorOnly ctx: Context):
  //ERROR:
  def f = sink(ctx)

class B(using ctx: Context):
  //OK:
  def g = log(ctx)
