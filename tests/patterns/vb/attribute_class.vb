' Class with Serializable attribute - should match
' ERROR:
<Serializable>
Public Class SerializableClass
  Public Sub Foo()
  End Sub
End Class

' Assembly (global) + Serializable (local) - should match
' Assembly attr is consumed globally; Serializable is correctly attached to the class
<Assembly: ComVisible(False)>
' ERROR:
<Serializable>
Public Class MixedAttrClass
  Public Sub Foo()
  End Sub
End Class

' Multiple local attributes including Serializable - should match
' ERROR:
<Serializable>
<CLSCompliant(True)>
Public Class MultipleLocalAttrsClass
  Public Sub Foo()
  End Sub
End Class

' Only assembly attribute (global) - should NOT match (no Serializable on the class)
<Assembly: ComVisible(False)>
' OK:
Public Class AssemblyOnlyClass
  Public Sub Foo()
  End Sub
End Class

' No attributes - should NOT match
' OK:
Public Class PlainClass
  Public Sub Foo()
  End Sub
End Class

' Serializable first, then assembly attribute - Serializable is class attr, should match
' ERROR:
<Serializable>
<Assembly: Guid("12345678-1234-1234-1234-123456789012")>
Public Class SerializableThenAssemblyClass
  Public Sub Foo()
  End Sub
End Class
