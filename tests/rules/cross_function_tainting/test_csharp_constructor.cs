class TaintedUser 
{
    private string key;

    public TaintedUser(string seller) 
    {
        this.key = source();
    }

    public void Props() 
    {
        // ruleid: csharp_constructor_sqli
        string query = "SELECT * FROM table WHERE name = " + this.key;
        return;
    }
}

class User 
{
    private string name;

    public User(string userName) 
    {
        this.name = userName;
    }
    
    public string GetProfile() 
    {
        // ruleid: csharp_constructor_sqli
        string query = "SELECT * FROM users WHERE name = " + this.name;
        return query;
    }
}

class FieldUser 
{
    public string name;

    public FieldUser() 
    {
        this.name = "";
    }
    
    public string GetProfile() 
    {
        // ruleid: csharp_constructor_sqli
        string query = "SELECT * FROM users WHERE name = " + this.name;
        return query;
    }
}

public class Program 
{
    public static void Main() 
    {
        string taintedInput = source();
        User user = new User(taintedInput);
        string result = user.GetProfile();
        
        // Test field assignment taint flow
        string taintedInput2 = source();
        FieldUser fieldUser = new FieldUser();
        fieldUser.name = taintedInput2;
        string fieldResult = fieldUser.GetProfile();
        
        return;
    }
}