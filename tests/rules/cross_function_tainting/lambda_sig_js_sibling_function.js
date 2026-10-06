// A lambda calls a function declared beside it in the same scope: the
// caller's parameter reaches the helper's sink through the call.
(function () {
    function format(str, args) {
        var keys = Object.keys(args), i;
        for (i = 0; i < keys.length; i++) {
            // ruleid: lambda-sig-js-sibling-function
            str = str.replace(new RegExp("\\{" + keys[i] + "\\}", "gi"), args[keys[i]]);
        }
        return str;
    }
    ctx.prototype.applyStyle = function (color) {
        this.el.setAttribute("fill", format("rgba({r},{g})", {r: color, g: 1}));
    };
}());
