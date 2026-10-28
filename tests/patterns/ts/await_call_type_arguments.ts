async function load(id: string) {
  // MATCH:
  const user = await client.get<User>(id);
  const order = await client.put<Order>(id);
}
