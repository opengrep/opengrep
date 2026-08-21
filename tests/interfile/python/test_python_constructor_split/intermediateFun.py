def intermediateFun ():
    tainted_input = source()
    user = User(tainted_input)
    return user
