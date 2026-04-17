// Lambda assigned as a property of an object literal, then called via
// unresolved property access. The parameter `data` matches the source
// pattern (concrete source), so `sink(data)` should fire regardless of
// whether the call graph can resolve `x.success(a)` back to the lambda.

function test1(a) {
    var x = {
        url: '/api/settings',
        success: function(data) {
            // ruleid: taint-func-param
            sink(data);
        }
    };
    x.success(a);
}
