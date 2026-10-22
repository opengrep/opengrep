<%@ WebService Language="VB" Class="Greeter" %>
Imports System.Web.Services

Public Class Greeter
    Inherits WebService

    <WebMethod()>
    Public Function Hello(name As String) As String
        ' ruleid: aspnet-web-service-vb
        Return "Hi " & name
    End Function

    <WebMethod()>
    Public Function Bye(name As String) As String
        ' ok: aspnet-web-service-vb
        Return "Bye " & name
    End Function
End Class
