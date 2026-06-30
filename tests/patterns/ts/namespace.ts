// a TS namespace is now a real module definition (not an IIFE)
// MATCH:
namespace MyApp {
  export const version = 1;
}
