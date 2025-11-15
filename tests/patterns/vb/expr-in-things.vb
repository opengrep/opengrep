Imports System

Module Program
    Sub Main()
        ' ERROR:
        foo()

        ' ERROR:
        If foo() Then
            Console.WriteLine("foo() returned True")
        End If

        ' ERROR:
        While foo()
            Console.WriteLine("Looping with foo()")
            Exit While
        End While

        ' ERROR:
        For i As Integer = 0 To 5 Step If(foo(), 2, 1)
            Console.WriteLine("i = " & i)
        Next

        ' ERROR:
        Select Case foo()
            Case True
                Console.WriteLine("foo() was True")
            Case False
                Console.WriteLine("foo() was False")
        End Select

        Try
            ' ERROR:
            Dim result = foo()
            Console.WriteLine("foo() in Try: " & result)
            	
        ' OK:
        Catch foo As Exception
            ' ERROR:
            foo()
        End Try
        
        ' ERROR:
        Dim arr() As Boolean = {foo(), foo(), foo()}

        ' ERROR:
        Console.WriteLine("foo() arg: " & foo())

        ' ERROR:
        Dim x As Boolean = (foo() AndAlso foo()) OrElse foo()

        ' ERROR:
        Dim f = Function() foo()
        Console.WriteLine("Lambda result: " & f())

        Dim o As New TestClass()
        ' ERROR:
        o.Prop = foo()

        With o
            ' ERROR:
            .Prop = foo()
        End With

        ' OK:
        Do
            Console.WriteLine("foo() in Do loop")
        ' ERROR:
        Loop Until foo()

    End Sub

    Function Helper() As Boolean
        ' ERROR:
        Return foo()
    End Function
End Module

Class TestClass
    Public Property Prop As Boolean
End Class

' OK:
Function foo() As Boolean
    Static toggle As Boolean = False
    toggle = Not toggle
    Return toggle
End Function

Sub xml_test
    Dim foo = <hello>
    <!-- ERROR: -->
    <ns:tag><%= foo() + 1 %></ns:tag>
    <!-- ERROR: -->
    <other_tag <%= goo(foo()) %>/>
    <!-- ERROR: -->
    <yet_another_tag attr=<%= goo(foo()) %>/>
    </hello>
End Sub
