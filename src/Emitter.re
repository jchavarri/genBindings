let emitExternal = (path, name, rest) => {j|[@bs.module "./$path"] external $name$rest;\n|j};

let emitType = t =>
  switch (t) {
  | DecodeFlowJson.Num => "float"
  | Str => "string"
  | Bool => "bool"
  | _ => failwith("unsupported type")
  };

let emitObjectFields = props =>
  props
  ->Belt.List.map(p =>
      switch (p) {
      | DecodeFlowJson.NamedProp(id, namedProp) =>
        /* y: float, */
        let annotation =
          switch (namedProp) {
          | Field(t, field) =>
            let typ = emitType(t);
            field.fldOptional ? {j|optional($typ)|j} : typ;
          | Method
          | Get
          | Set => failwith("unsupported named prop type")
          };
        id ++ ": " ++ annotation;
      | IndexProp /* ['y']: float, */
      | CallProp => ""
      }
    )
  ->Belt.List.keep(s => Js.String.length(s) > 0)
  |> String.concat(", ");

let fromType = (typ, declarationName, baseName) => {
  let reasonDecName = Js.String.toLowerCase(declarationName);
  let originalName = reasonDecName !== declarationName ? declarationName : "";
  switch (typ) {
  | DecodeFlowJson.Str =>
    emitExternal(baseName, reasonDecName, {j|: string = "$originalName"|j})
    ++ "\n"
  | Obj(obj) =>
    let recordFields = emitObjectFields(obj.objProps);
    emitExternal(
      baseName,
      reasonDecName,
      {j|: Js.t({. $recordFields }) = "$originalName"|j},
    )
    ++ "\n";
  | Generic(symbol, structural, typeArgsOpt) =>
    emitExternal(baseName, reasonDecName, {j|: Js.t('a) = ""|j}) ++ "\n"
  | Fun(f) =>
    let hasAny =
      f.funParams
      ->Belt.List.some(p =>
          switch (p) {
          | Any => true
          | _ => false
          }
        );
    hasAny ?
      {j|/* Exported function "$reasonDecName" has parameter types or returned type "any" and can't be exported. */\n\n|j} :
      {
        let paramTypes =
          f.funParams->Belt.List.map(emitType) |> String.concat(", ");
        let returnType = f.funReturn->emitType;
        emitExternal(
          baseName,
          reasonDecName,
          {j|: ($paramTypes) => $returnType = "$originalName"|j},
        )
        ++ "\n";
      };
  | _ => ""
  };
};