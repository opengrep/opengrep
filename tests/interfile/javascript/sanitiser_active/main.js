function main() {
  const clean = source();
  sendClean(clean);
  const dirty = source();
  sendDirty(dirty);
}
