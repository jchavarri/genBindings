type spawnSyncConfig;
[@bs.obj]
external makeConfig:
  (~encoding: [@bs.string] [ | `utf8], unit) => spawnSyncConfig =
  "";
[@bs.module "child_process"]
external spawnSync:
  (string, array(string), spawnSyncConfig) => {. "stdout": string} =
  "dirname";

let typeAliasesMap = Belt.Map.String.empty;

let getTypeForLoc = loc => {
  let res = typeAliasesMap->Belt.Map.String.get(loc);
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
        /* fileName,
           String(line),
           String(column), */
      |],
      makeConfig(~encoding=`utf8, ()),
    )##stdout;
  };
};