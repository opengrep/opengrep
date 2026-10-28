// @flow
function show(props: {title: string, ...}) {
  // MATCH:
  render(props.title);
  log(props.title);
}
