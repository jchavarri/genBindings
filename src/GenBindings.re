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
          | _ => failwith("unsupported named prop type")
          };
        id ++ ": " ++ annotation;
      | IndexProp /* ['y']: float, */
      | CallProp => ""
      }
    )
  ->Belt.List.keep(s => Js.String.length(s) > 0)
  |> String.concat(", ");

let fromJson = (json, baseName, declarationName) => {
  let parsedType = DecodeFlowJson.decode(json);
  let reasonDecName = Js.String.toLowerCase(declarationName);
  let originalName = reasonDecName !== declarationName ? declarationName : "";
  switch (parsedType) {
  | Str =>
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
  | _ => ""
  };
};