//ERROR:
@JsonSerialize(using = classOf[DataTypeSerializer])
class A

//OK:
@JsonSerialize(as = classOf[DataTypeSerializer])
class B
