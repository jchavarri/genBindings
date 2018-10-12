type coord = { x: float, y: float, z: option(float) };

[@bs.module "./input.js"] external origin: Js.t({. x: float, y: float, z: float }) = "";

[@bs.module "./input.js"] external origin2: Js.t('a) = "";
let origin2 = {x: origin2##x, y: origin2##y, z: origin2##z};