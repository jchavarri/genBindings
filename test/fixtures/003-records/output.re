type coord = { x: float, y: float, z: optional(float) };

/* Exported value "origin" has type "Object", which can't be represented in Reason / OCaml. */
/* This can be fixed by explicitly annotating the exported value with a type that has been previously defined */

[@bs.module "./input.js"] external origin2: Js.t('a) = "";
let origin2 = {x: origin2##x, y: origin2##y, z: origin2##z};