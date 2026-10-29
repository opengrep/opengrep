<%@ WebHandler Language="C#" Class="Echo" %>
using System.Web;

public class Echo : IHttpHandler {
    public void ProcessRequest(HttpContext context) {
        // ruleid: aspnet-handler-csharp
        context.Response.Write(context.Request.QueryString["q"]);
        // ok: aspnet-handler-csharp
        context.Response.Clear();
    }

    public bool IsReusable { get { return true; } }
}
