class CreateService:
    def __init__(self, user, data):
        self.user = user
        self.data = data

    def execute(self, group):
        return self.data
