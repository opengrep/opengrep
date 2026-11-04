function make(url) {
  return new Client(url);
}

run(async () => {
  let client = await make(source());
  const reply = await client.get();
  // ruleid: js_lambda_local_is_not_captured
  sink(reply.body);
});

run(async () => {
  let client = await make("fixed");
  const reply = await client.get();
  // ok: js_lambda_local_is_not_captured
  sink(reply.body);
});
