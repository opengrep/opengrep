class Impl {
  produce(): string {
    return source();
  }
}

class Other {
  produce(): string {
    return "constant";
  }
}

function viaUndefined(other: Impl | undefined) {
  // ruleid: ts_receiver_union_type
  sink(other.produce());
}

function viaNull(other: Impl | null) {
  // ruleid: ts_receiver_union_type
  sink(other.produce());
}

function viaOtherUndefined(other: Other | undefined) {
  // ok: ts_receiver_union_type
  sink(other.produce());
}
