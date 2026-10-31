class Handler {
  @((): MethodDecorator => (target, key, descriptor) =>
    // MATCH:
    Reflect.defineMetadata("pattern", key, descriptor))
  handle() {}
  @Plain()
  other() {}
}
