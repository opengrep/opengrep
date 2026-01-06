// Test: JavaScript collection models for taint propagation

function testArrayPush(tainted) {
    const arr = [];
    arr.push(tainted);
    // ruleid: js-collection-taint
    sink(arr);
}

function testMapSet(tainted) {
    const map = new Map();
    map.set("key", tainted);
    const value = map.get("key");
    // ruleid: js-collection-taint
    sink(value);
}

function testSetAdd(tainted) {
    const set = new Set();
    set.add(tainted);
    // ruleid: js-collection-taint
    sink(set);
}

function testArrayPop(tainted) {
    const arr = [tainted];
    const value = arr.pop();
    // ruleid: js-collection-taint
    sink(value);
}

function sink(data) {
    console.log(data);
}
