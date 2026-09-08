import { run } from "./sinks.mjs";
import { other } from "lib/other";
const cjs = require("./lib.cjs");

run(source());
other(source());
cjs.fromRequire(source());
