class IntermethodClass:
    def taint_method(self):
        return source()

    def sink_method(self):
        # ruleid:python_constructor_sqli
        query = f"SELECT * FROM users WHERE name = {self.taint_method()}"
        return query

