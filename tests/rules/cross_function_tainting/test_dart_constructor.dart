class TaintedUser {
  String key;

  TaintedUser(String seller) {
    this.key = source();
  }

  void props() {
    // ruleid: dart_constructor_taint
    sink(this.key);
  }
}

class User {
  String name;

  User(String userName) {
    this.name = userName;
  }

  String getProfile() {
    return this.name;
  }
}

void testImplicitNew() {
  var taintedInput = source();
  var user = User(taintedInput);
  // ruleid: dart_constructor_taint
  sink(user.getProfile());
}

void testExplicitNew() {
  var taintedInput = source();
  var user = new User(taintedInput);
  // ruleid: dart_constructor_taint
  sink(user.getProfile());
}

void testChainedConstructorCall() {
  var profile = User(source()).getProfile();
  // ruleid: dart_constructor_taint
  sink(profile);
}

void testCleanConstructor() {
  var user = User("clean");
  sink(user.getProfile());
}
