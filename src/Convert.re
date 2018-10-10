let typeFromFlowKind = kind =>
  switch (kind) {
  | "Num" => "float"
  | "Str" => "string"
  | _ => raise(Failure("Invalid type from Flow"))
  };
/*
let typeFromProp = prop /* : {kind, type, polarity, optional} */ => {
  let typ_ = typeFromFlowKind(prop.typ_.kind);
  prop.optional ? {j|optional($typ_)|j} : typ_;
}; */