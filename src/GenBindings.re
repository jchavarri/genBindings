let fromJson = (json, baseName, declarationName) => {
  let parsedType = DecodeFlowJson.decode(json);
  Emitter.fromType(parsedType, declarationName, baseName);
};