class User:
    def __init__(self, user_name):
        self.name = user_name
    
    def get_profile(self):
        # ruleid:python_constructor_sqli
        query = f"SELECT * FROM users WHERE name = {self.name}"
        return query

