Public Class Test
    Public Shared Sub Test()
        'ERROR:
        userData = [Get]()
        Console.WriteLine("do stuff")
        FooBar()
        Eval(userData)
        FooBar()
    End Sub
    
    Public Shared Sub Test()
        userData = [Get]()
        Console.WriteLine("do stuff")
        FooBar()
        Eval(userDataNotReallyTheSameVar)
        FooBar()
    End Sub
End Class
