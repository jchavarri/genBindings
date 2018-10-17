let rec emitType = t =>
  switch (t) {
  | DecodeFlowJson.Num => "float"
  | Str => "string"
  | Bool => "bool"
  | Obj(obj) =>
    let recordFields = emitObjectFields(obj.objProps);
    {j|{ $recordFields }|j};
  | TypeAlias(_)
  | Fun(_)
  | Generic(_, _, _)
  | Any => failwith("unsupported type")
  }
and emitObjectFields = props =>
  props
  ->Belt.List.map(p =>
      switch (p) {
      | DecodeFlowJson.NamedProp(id, namedProp) =>
        /* y: float, */
        let annotation =
          switch (namedProp) {
          | Field(t, field) =>
            let typ = emitType(t);
            field.fldOptional ? {j|option($typ)|j} : typ;
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

let emitExternal = (path, name, rest) => {j|[@bs.module "./$path"] external $name$rest;\n|j};
let emitTypeDeclaration = (name, t) => {
  let afterName =
    switch (t) {
    | Some(t) =>
      let e = emitType(t);
      {j| = $e|j};
    | None => ""
    };
  {j|type $name$afterName;\n|j};
};

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
  | Generic(Symbol(provenance, name), _structural, _typeArgsOpt) =>
    let maybeType =
      switch (provenance) {
      | Local(loc)
      | Imported(loc)
      | Remote(loc)
      | Library(loc) =>
        SymbolStore.getTypeAtPosition({
          SymbolStore.filePath: loc.source,
          line: loc.start.line,
          typeName: name,
        })
      | Builtin => None
      };
    let conversion =
      (
        switch (maybeType) {
        | Some(typ) =>
          switch (typ) {
          | Obj(obj) =>
            let converterFromType =
              obj.objProps
              ->Belt.List.map(p =>
                  switch (p) {
                  | DecodeFlowJson.NamedProp(id, _namedProp) =>
                    /* y: float, */
                    {j|$id: $reasonDecName##$id|j}
                  | IndexProp /* ['y']: float, */
                  | CallProp => ""
                  }
                )
              |> String.concat(", ");
            "let " ++ reasonDecName ++ " = {" ++ converterFromType ++ "};";
          | _ => ""
          }
        | None => ""
        }
      )
      ++ "\n";

    emitExternal(baseName, reasonDecName, {j|: Js.t('a) = ""|j})
    ++ conversion;
  | TypeAlias({taName: _, taTparams: _, taType}) =>
    emitTypeDeclaration(reasonDecName, taType) ++ "\n"
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