from FieldUser import FieldUser
from IntermethodClass import IntermethodClass
from User import User
from intermediateFun import intermediateFun
from sink_ex import sink_ex

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

    # Test chained method call: Constructor(tainted).method()
    # ruleid:python_constructor_sqli
    chained_result = f"SELECT * FROM users WHERE name = {User(source()).get_profile()}"

    return result
