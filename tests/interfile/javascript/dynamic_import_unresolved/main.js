export async function go() {
  const m = await import("./a");
  m.run(source());
}
