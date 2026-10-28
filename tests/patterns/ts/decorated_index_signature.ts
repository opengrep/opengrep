// MATCH:
class Table {
  @Indexed() [key: string]: number;
}
class Plain {
  [key: string]: number;
}
