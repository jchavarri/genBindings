/* This file is in essence the opposite of json_of_t: https://github.com/facebook/flow/blob/938105c839a8a02b674b0c20191a0ea73bf71724/src/common/ty/ty_debug.ml#L204-L395 */

open Json.Decode;

/* Subset of https://github.com/facebook/flow/blob/7628bde730ba2464dde9a605b062adeff5df84e6/src/common/ty/ty.ml#L11-L33 */
type t =
  | TypeAlias
  | Obj(objT)
  | Generic
  | Fun(funT)
  | Num
  | Str
  | Bool
  | Any
and polarity =
  | Positive
  | Negative
  | Neutral
and identifier = string
and field = {
  fldPolarity: polarity,
  fldOptional: bool,
}
and funParam = {prmOptional: bool}
and typeParam = {
  tpName: identifier,
  tpBound: option(t),
  tpPolarity: polarity,
  tpDefault: option(t),
}
and funT = {
  funTypeParams: option(list(typeParam)),
  funParams: list(t), /* list((option(identifier), t, funParam)), */
  funRestParam: option((option(identifier), t)),
  funReturn: t,
}
and namedProp =
  | Field(t, field)
  | Method /*(funT)*/
  | Get /*(t)*/
  | Set /*(t)*/
and dict = {
  dictPolarity: polarity,
  dictName: option(identifier),
  dictKey: t,
  dictValue: t,
}
and prop =
  | NamedProp(identifier, namedProp)
  | IndexProp /* (dict) */
  | CallProp /* (funT) */
and objT = {
  objExact: bool,
  objFrozen: bool,
  objProps: list(prop),
};

let decodePolarity = json =>
  json
  |> field("polarity", string)
  |> (
    a =>
      switch (a) {
      | "Negative" => Negative
      | "Neutral" => Neutral
      | "Positive" => Positive
      | _ =>
        raise(Failure("Invalid type for polarity when decoding Flow json"))
      }
  );
let decodeField = json => {
  fldPolarity: json |> decodePolarity,
  fldOptional: json |> field("optional", bool),
};

let rec innerNamedPropDecoder = json =>
  json
  |> field("prop", json =>
       json
       |> field("kind", string)
       |> (
         a =>
           switch (a) {
           | "field" =>
             Field(json |> field("type", decode), json |> decodeField)
           | "Method" => Method
           | "Get" => Get
           | "Set" => Set
           | _ =>
             raise(Failure("Invalid type for prop when decoding Flow json"))
           }
       )
     )
and namedPropDecoder = json =>
  json
  |> field("prop", json =>
       NamedProp(
         json |> field("name", string),
         json |> innerNamedPropDecoder,
       )
     )
and propDecoder = json =>
  json
  |> field("kind", string)
  |> (
    a =>
      switch (a) {
      | "NamedProp" => json |> namedPropDecoder
      | "IndexProp" => IndexProp
      | "CallProp" => CallProp
      | _ => raise(Failure("Invalid type for prop when decoding Flow json"))
      }
  )
and typeParamDecoder = json => {
  tpName: json |> field("name", string),
  tpBound: json |> optional(field("bound", decode)),
  tpPolarity: json |> decodePolarity,
  tpDefault: json |> optional(field("default", decode)),
}
and objTDecode = json => {
  objExact: json |> field("exact", bool),
  objFrozen: json |> field("frozen", bool),
  objProps:
    json |> field("props", array(propDecoder) |> map(Array.to_list)),
}
and funTDecode = json => {
  funTypeParams:
    json
    |> field(
         "typeParams",
         optional(array(typeParamDecoder) |> map(Array.to_list)),
       ),
  funParams:
    json |> field("paramTypes", array(decode) |> map(Array.to_list)),
  funRestParam:
    json
    |> field(
         "restParam",
         optional(
           pair(
             optional(field("restParamName", string)),
             field("restParamType", decode),
           ),
         ),
       ),
  funReturn: json |> field("returnType", decode),
}
and decode = json =>
  json
  |> field("kind", string)
  |> (
    a =>
      switch (a) {
      | "TypeAlias" => TypeAlias
      | "Obj" => Obj(objTDecode(json))
      | "Generic" => Generic
      | "Fun" => Fun(funTDecode(json))
      | "Num" => Num
      | "Str" => Str
      | "Bool" => Bool
      | "Any" => Any
      | _ =>
        raise(
          Failure("Invalid type for type 'kind' when decoding Flow json"),
        )
      }
  );