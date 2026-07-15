// With an ellipsis metavariable, fields are matched in order, so only
// the object where key2 comes first matches. Compare with
// unordered_fields3.js: the same objects with a plain pattern match
// regardless of key order.
//ERROR: match
var a = {
    key2: value2,
    key1: value1,
};
var b = {
    key1: value1,
    key2: value2,
};
