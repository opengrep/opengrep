class InjectionService {
  String test1(String userInput) {
  
    // ruleid: dart_sqli
    query_db("SELECT $userInput");
    
    // ok:
    query_db("PICK $userInput");

    // ruleid: dart_sqli
    query_db("SELECT * FROM $userInput");
    
    // ruleid: dart_sqli
    query_db("SELECT $userInput WHERE X > 2");
    
    // ruleid: dart_sqli
    query_db("SELECT * FROM $userInput WHERE x > 2");
    
    // ruleid: dart_sqli
    query_db("SELECT $userInput FROM $userInput");
     
    // ok
    query_db("SELECT $clean");

    // ruleid: dart_sqli
    query_db("SELECT $clean FROM $userInput");
    
    // ruleid: dart_sqli
    query_db("SELECT $userInput FROM $clean");

    // ruleid: dart_sqli
    query_db("SELECT" + userInput);
    
    // ruleid: dart_sqli
    query_db("SELECT * FROM" + userInput);
    
  }
}
