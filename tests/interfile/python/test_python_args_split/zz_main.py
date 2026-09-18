from foo1 import foo1
from foo10 import foo10
from foo11 import foo11
from foo12 import foo12
from foo2 import foo2
from foo3 import foo3
from foo4 import foo4
from foo5 import foo5
from foo6 import foo6
from foo7 import foo7
from foo8 import foo8
from foo9 import foo9

foo1(source(), 0, 1)

#####################################

foo2(0, source(), 1)

#####################################

foo3(0, 1, source())

#####################################

foo4(0, 1, danger=source())

#####################################

foo5(0, 1, danger=source())

#####################################

foo6(0, 1, danger=source())

#####################################

foo7(0, ok1=1, danger=source())

#####################################

foo8(0, ok1=1, danger=source())

#####################################

foo9(0, ok1=1, danger=source())

#####################################

foo10(0, danger=source(), ok1=1)

#####################################

foo11(0, danger=source(), ok1=1)

#####################################

foo12(ok2=0, danger=source(), ok1=1)
