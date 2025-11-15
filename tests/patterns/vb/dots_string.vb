Sub Bar(input As Integer)
    ' ERROR:
    Foo("whatever sequence of chars")

    ' OK:
    Foo("not a constant string: " & input)
End Sub

