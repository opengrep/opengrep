async function load(id: string) {
  // MATCH:
  const user = await client.get<User>(id).catch(fallback);
  const order = (await client.get<Order>(id)).catch(fallback);
}
