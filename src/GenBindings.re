let fromJson = (json, filePath, line, declarationName) => {
  let parsedType = DecodeFlowJson.decode(json);

  /* Store only aliases. They will be used inside Emitter */
  switch (parsedType) {
  | TypeAlias({taName: Symbol(_, name), taTparams: _, taType: Some(typ_)}) =>
    SymbolStore.setTypeAtPosition({filePath, line, typeName: name}, typ_)
  | _ => ()
  };

  Emitter.fromType(
    parsedType,
    declarationName,
    Node.Path.basename(filePath),
  );
};