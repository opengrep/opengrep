// A for-in pattern must match for-in and NOT for-of. Both loops below have the
// same body shape, so only the loop keyword can tell them apart.
function copyKeys(src, target) {
    //ERROR: match
    for (const k in src) {
        target[k] = src[k];
    }
    return target;
}

function copyItems(list, target) {
    // for-of is a different loop and must not match a for-in pattern.
    for (const k of list) {
        target[k] = list[k];
    }
    return target;
}
