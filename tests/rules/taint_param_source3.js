function test() {
    var foobar = function(req, res, next) {
        var buf = '';
        req.on('data', function (chunk) {
            buf += chunk
        });
        // todoruleid: test
        // TODO: callback writes through captured outer variables are not modeled yet.
        sink(buf);
    };
}
