type spawnSyncConfig;
[@bs.obj]
external makeConfig:
  (~encoding: [@bs.string] [ | `utf8], unit) => spawnSyncConfig =
  "";
[@bs.module "child_process"]
external spawnSync:
  (string, array(string), spawnSyncConfig) => {. "stdout": string} =
  "";

type position = {
  filePath: string,
  line: int,
  typeName: string,
};

let typesMap: Belt.MutableMap.String.t(DecodeFlowJson.t) =
  Belt.MutableMap.String.make();

let keyGeneration = p => p.filePath ++ string_of_int(p.line) ++ p.typeName;

let getTypeAtPosition = position =>
  typesMap->Belt.MutableMap.String.get(keyGeneration(position));

let setTypeAtPosition = (position, typ_) =>
  typesMap->Belt.MutableMap.String.set(keyGeneration(position), typ_);