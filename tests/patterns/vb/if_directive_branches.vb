Module M
    Sub Bar
        ' Statements in both #If and #Else branches should be matched

        #If DEBUG
        ' ERROR:
        Foo(x)
        #Else
        ' ERROR:
        Foo(y)
        #End If

        ' All three branches of #ElseIf should be matched too
        #If RELEASE
        ' ERROR:
        Foo(a)
        #ElseIf PROFILE
        ' ERROR:
        Foo(b)
        #Else
        ' ERROR:
        Foo(c)
        #End If

        ' A call without a directive is also matched
        ' ERROR:
        Foo(z)
    End Sub
End Module
