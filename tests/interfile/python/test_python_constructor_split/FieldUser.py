class FieldUser:
    def __init__(self):
        self.name = ""
    
    def get_profile(self):
        # ruleid:python_constructor_sqli
        query = f"SELECT * FROM users WHERE name = {self.name}"
        return query

