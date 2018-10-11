/* This file is in essence the opposite of json_of_t: https://github.com/facebook/flow/blob/938105c839a8a02b674b0c20191a0ea73bf71724/src/common/ty/ty_debug.ml#L204-L395 */

open Json.Decode;

/* Subset of https://github.com/facebook/flow/blob/7628bde730ba2464dde9a605b062adeff5df84e6/src/common/ty/ty.ml#L11-L33 */
type t =
  | TypeAlias
  | Str
  | Obj(objT)
  | Generic
  | Fun
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
  funParams: list((option(identifier), t, funParam)),
  funRestParam: option((option(identifier), t)),
  funReturn: t,
  funTypeParams: option(list(typeParam)),
}
and namedProp =
  | Field(t, field)
  | Method(funT)
  | Get(t)
  | Set(t)
and dict = {
  dictPolarity: polarity,
  dictName: option(identifier),
  dictKey: t,
  dictValue: t,
}
and prop =
  | NamedProp(identifier) /*, namedProp) */
  | IndexProp /* (dict) */
  | CallProp /* (funT) */
and objT = {
  objExact: bool,
  objFrozen: bool,
  objProps: list(prop),
};

let namedPropDecoder =
  field("prop", json => NamedProp(json |> field("name", string)));

let propDecoder =
  field("kind", string)
  |> andThen(
       fun
       | "NamedProp" => namedPropDecoder
       | "IndexProp" => (_ => IndexProp)
       | "CallProp" => (_ => CallProp)
       | _ =>
         raise(Failure("Invalid type for prop when decoding Flow json")),
     );

let objDecode = json =>
  Obj({
    objExact: json |> field("exact", bool),
    objFrozen: json |> field("frozen", bool),
    objProps:
      json |> field("props", array(propDecoder) |> map(Array.to_list)),
  });

let decode =
  field("kind", string)
  |> andThen(
       fun
       | "TypeAlias" => (_ => TypeAlias)
       | "Str" => (_ => Str)
       | "Obj" => objDecode
       | "Generic" => (_ => Generic)
       | "Fun" => (_ => Fun)
       | _ =>
         raise(
           Failure("Invalid type for type 'kind' when decoding Flow json"),
         ),
     );