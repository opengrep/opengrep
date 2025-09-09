class Tainted_user:
    def __init__(self, seller):
        self.key = source()
    def props(self):
        __init__(self, taint)
    # ruleid: python_constructor_sqli
        query = f"SELECT * FROM table WHERE name = {self.key}"
        return query

class User:
    def __init__(self, user_name):
        self.name = user_name
    
    def get_profile(self):
        # ruleid:python_constructor_sqli
        query = f"SELECT * FROM users WHERE name = {self.name}"
        return query

def intermediateFun ():
    tainted_input = source()
    user = User(tainted_input)
    return user
def sink_ex(user):
    return user.get_profile()
class FieldUser:
    def __init__(self):
        self.name = ""
    
    def get_profile(self):
        # ruleid:python_constructor_sqli
        query = f"SELECT * FROM users WHERE name = {self.name}"
        return query

class IntermethodClass:
    def taint_method(self):
        return source()

    def sink_method(self):
        # ruleid:python_constructor_sqli
        query = f"SELECT * FROM users WHERE name = {self.taint_method()}"
        return query

def main():
    user = intermediateFun()
    result = sink_ex(user)

    # Test field assignment taint flow
    tainted_input = source()
    field_user = FieldUser()
    field_user.name = tainted_input
    field_result = field_user.get_profile()

    # Test intermethod taint flow
    intermethod_obj = IntermethodClass()
    intermethod_result = intermethod_obj.sink_method()

    return result
