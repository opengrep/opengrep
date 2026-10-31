// @flow
// MATCH:
class Socket {
  +url: string = "";
  static +timeout: number = 0;
}
class Plain {
  timeout: number = 0;
}
