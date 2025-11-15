Sub Foo(strArg As String, intArg as Integer)

Dim strVar As String
Dim intVar as Integer

' OK:
foo(strArg)

' OK:
foo(strVar)

' ERROR:
foo(intArg)

' ERROR:
foo(intVar)

Dim intInit = 3

' ERROR:
foo(intInit)

End Sub
