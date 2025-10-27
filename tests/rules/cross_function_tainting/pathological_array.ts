function source(): string {
  return "tainted";
}

function sink(x: any) {
  console.log(x);
}

function bad(input: string) {
  const obj: any = {};

  for (item of input) {
    const key = item;
    obj[item] = [obj[item], item];
  }

  return obj;
}

// ruleid: test-pathological-array
sink(bad(source()));
