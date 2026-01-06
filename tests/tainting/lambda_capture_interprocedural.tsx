// Test: Lambda captures tainted variable and passes to interprocedural sink
// With taint_intrafile: 1 finding
// The taint should flow from props through the arrow function to callee's sink

function Caller(props: { url: string }) {
  return <button onClick={() => callee(props.url)} />;
}

function callee(input: string) {
  // ruleid: lambda-capture-interprocedural
  window.location.href = input;
}
