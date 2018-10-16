type spawnSyncConfig;
[@bs.obj]
external makeConfig:
  (~encoding: [@bs.string] [ | `utf8], unit) => spawnSyncConfig =
  "";
[@bs.module "child_process"]
external spawnSync:
  (string, array(string), spawnSyncConfig) => {. "stdout": string} =
  "";

let typesMap = Belt.Map.String.empty;

let getTypeForLoc = (appearanceLoc, provenanceLoc: Loc.t) => {
  let res = typesMap->Belt.Map.String.get(appearanceLoc);
  switch (res) {
  | Some(t) => t
  | None =>
    let flowPath = "node_modules/.bin/flow";
    if (!Node.Fs.existsSync(flowPath)) {
      failwith({j|genBindings: Couldn't find flow at path $flowPath.|j});
    };
    spawnSync(
      flowPath,
      [|
        "type-at-pos",
        "--expand-json-output",
        provenanceLoc.source,
        string_of_int(provenanceLoc.start.line),
        string_of_int(provenanceLoc.start.column),
      |],
      makeConfig(~encoding=`utf8, ()),
    )##stdout;
  };
};