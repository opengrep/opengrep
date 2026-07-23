// An ellipsis metavariable behaves like `...`: fields are matched in any
// order, and $...REST is bound to the fields the rest of the pattern did
// not match, wherever those sit. Both objects below match.
//ERROR: match
var a = {
    key2: value2,
    key1: value1,
};
//ERROR: match
var b = {
    key1: value1,
    key2: value2,
};
