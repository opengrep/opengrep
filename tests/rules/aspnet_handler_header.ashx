<%@ Assembly Name="System.Web" %>
<% @ webhandler
     language='c#'
     class=Echo description="echo %> handler" %>using System.Web;

public class Echo : IHttpHandler {
    public void ProcessRequest(HttpContext context) {
        // ruleid: aspnet-handler-header
        context.Response.Write(context.Request.QueryString["q"]);
        // ok: aspnet-handler-header
        context.Response.Clear();
    }

    public bool IsReusable { get { return true; } }
}
