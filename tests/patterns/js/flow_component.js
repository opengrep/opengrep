// @flow
// MATCH:
component Card(title: string, 'aria-label' as label: string) {
  return <div>{title}</div>;
}
function Plain(props) {
  return null;
}
