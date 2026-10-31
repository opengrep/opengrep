async function h() {
  // MATCH:
  const a = await f<T>;
  const b = await g<T>;
}
