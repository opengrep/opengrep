function f(files, lines) {
  // MATCH:
  for (using file of files) {
    read(file);
  }
  for (const line of lines) {
    write(line);
  }
}
