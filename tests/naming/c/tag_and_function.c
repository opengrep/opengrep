struct stat {
  int size;
};

int stat(const char *path, struct stat *buf);

int size_of(const char *path) {
  struct stat st;
  stat(path, &st);
  return st.size;
}
