class TaintedUser {
    private var key: String

    init(seller: String) {
        self.key = source()
    }

    func props() {
        // ruleid: swift_constructor_sqli
        let query = "SELECT * FROM table WHERE name = \(self.key)"
        return
    }
}

class User {
    private var name: String

    init(userName: String) {
        self.name = userName
    }
    
    func getProfile() -> String {
        // ruleid: swift_constructor_sqli
        let query = "SELECT * FROM users WHERE name = \(self.name)"
        return query
    }
}

func main() {
    let taintedInput = source()
    let user = User(userName: taintedInput)
    let result = user.getProfile()
    return
}