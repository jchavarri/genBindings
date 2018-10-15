/* From https://github.com/facebook/flow/blob/41b0eab99cdc5199421f7cccad9e0c4950f8b2f9/src/parser/loc.ml */

type position = {
  line: int,
  column: int,
  offset: int,
};

type t = {
  source: string, /* File_key.t option; */
  start: position,
  _end: position,
};