class R {
  constructor(path, other) {
    this.path = path;
    this.other = other;
  }
}

function iterated() {
  const infos = [];
  infos.push(new R(source(), "x"));
  for (const i of infos) {
    // ruleid: collection_element_fields_javascript
    sink(i.path);
    // ok: collection_element_fields_javascript
    sink(i.other);
  }
}

function elementRead() {
  const infos = [];
  infos.push(new R(source(), "x"));
  const e = infos.pop();
  // ruleid: collection_element_fields_javascript
  sink(e.path);
  // ok: collection_element_fields_javascript
  sink(e.other);
}

function indexed() {
  const infos = [];
  infos.push(new R(source(), "x"));
  // ruleid: collection_element_fields_javascript
  sink(infos[0].path);
  // ok: collection_element_fields_javascript
  sink(infos[0].other);
}

function callback() {
  const infos = [];
  infos.push(new R(source(), "x"));
  infos.forEach((i) => {
    // ruleid: collection_element_fields_javascript
    sink(i.path);
    // ok: collection_element_fields_javascript
    sink(i.other);
  });
}

function wholeValue() {
  const infos = [];
  infos.push(source());
  // ruleid: collection_element_fields_javascript
  sink(infos.join(","));
}
