// MATCH:
const a = <div>
  {x}
</div>;
// MATCH:
const b = <div> {x} </div>;
// MATCH:
const c = <div>{x}</div>;
const d = <div> {y} </div>;
