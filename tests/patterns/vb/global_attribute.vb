' Assembly attribute (global) before class - class should still be matchable
<Assembly: ComVisible(False)>
' ERROR:
Public Class AfterAssemblyAttr
  Public Sub Foo()
  End Sub
End Class

' Module attribute (global) before class - class should still be matchable
<Module: CLSCompliant(True)>
' ERROR:
Public Class AfterModuleAttr
  Public Sub Foo()
  End Sub
End Class

' Multiple assembly attributes before class - class should still be matchable
<Assembly: AssemblyTitle("TestApp")>
<Assembly: AssemblyVersion("1.0.0.0")>
<Assembly: AssemblyDescription("A test application")>
' ERROR:
Public Class AfterMultipleAssemblyAttrs
  Public Sub Foo()
  End Sub
End Class

' Assembly + Module attributes together before class
<Assembly: ComVisible(False)>
<Module: CLSCompliant(True)>
' ERROR:
Public Class AfterAssemblyAndModuleAttrs
  Public Sub Foo()
  End Sub
End Class

' Global attributes between two class declarations
' ERROR:
Public Class FirstClass
  Public Sub Foo()
  End Sub
End Class

<Assembly: ComVisible(False)>
' ERROR:
Public Class SecondClassAfterGlobalAttr
  Public Sub Foo()
  End Sub
End Class

' Local (non-global) attribute before class - class should also match simple class pattern
' ERROR:
<Serializable>
Public Class LocalAttrClass
  Public Sub Foo()
  End Sub
End Class

' Plain class with no attributes - should match
' ERROR:
Public Class PlainClass
  Public Sub Foo()
  End Sub
End Class
