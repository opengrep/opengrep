def foo1(danger, ok1, ok2):
    # ruleid: taint
    sink(danger)
    # ok:
    sink(ok1)
    # ok:
    sink(ok2)

foo1(source(), 0, 1)

#####################################

def foo2(ok1, danger, ok2):
    # ok:
    sink(ok1)
    # ruleid: taint
    sink(danger)
    # ok:
    sink(ok2)

foo2(0, source(), 1)

#####################################

def foo3(ok1, ok2, danger):
    # ok:
    sink(ok1)
    # ok:
    sink(ok2)
    # ruleid: taint
    sink(danger)

foo3(0, 1, source())

#####################################

def foo4(danger, ok1, ok2):
    # ruleid: taint
    sink(danger)
    # ok:
    sink(ok1)
    # ok:
    sink(ok2)

foo4(0, 1, danger=source())

#####################################

def foo5(ok1, danger, ok2):
    # ok:
    sink(ok1)
    # ruleid: taint
    sink(danger)
    # ok:
    sink(ok2)

foo5(0, 1, danger=source())

#####################################

def foo6(ok1, ok2, danger):
    # ok:
    sink(ok1)
    # ok:
    sink(ok2)
    # ruleid: taint
    sink(danger)

foo6(0, 1, danger=source())

#####################################

def foo7(danger, ok1, ok2):
    # ruleid: taint
    sink(danger)
    # ok:
    sink(ok1)
    # ok:
    sink(ok2)

foo7(0, ok1=1, danger=source())

#####################################

def foo8(ok1, danger, ok2):
    # ok:
    sink(ok1)
    # ruleid: taint
    sink(danger)
    # ok:
    sink(ok2)

foo8(0, ok1=1, danger=source())

#####################################

def foo9(ok1, ok2, danger):
    # ok:
    sink(ok1)
    # ok:
    sink(ok2)
    # ruleid: taint
    sink(danger)

foo9(0, ok1=1, danger=source())

#####################################

def foo10(ok1, danger, ok2):
    # ok:
    sink(ok1)
    # ruleid: taint
    sink(danger)
    # ok:
    sink(ok2)

foo10(0, danger=source(), ok1=1)

#####################################

def foo11(ok1, ok2, danger):
    # ok:
    sink(ok1)
    # ok:
    sink(ok2)
    # ruleid: taint
    sink(danger)

foo11(0, danger=source(), ok1=1)

#####################################

def foo12(ok1, ok2, danger):
    # ok:
    sink(ok1)
    # ok:
    sink(ok2)
    # ruleid: taint
    sink(danger)

foo12(ok2=0, danger=source(), ok1=1)
