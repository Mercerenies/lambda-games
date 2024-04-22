// Copyright 2024 Silvio Mayolo
//
// Lambdagames is free software: you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Lambdagames. If not, see
// <https://www.gnu.org/licenses/>.

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
