
"use strict";

class LambdaGamesError extends Error {

  constructor(message = "", ...args) {
    super(message, ...args);
    this.name = "LambdaGamesError";
    this.message = message;
  }

  toString() {
    return this.message;
  }

}

// FFI function to produce a custom Javascript error object, just to
// make our error messages a bit neater.
export function toError(s) {
  return new LambdaGamesError(s);
}
