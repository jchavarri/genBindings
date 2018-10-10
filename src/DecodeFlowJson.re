open Json.Decode;

/* Subset of https://github.com/facebook/flow/blob/7628bde730ba2464dde9a605b062adeff5df84e6/src/common/ty/ty.ml#L11-L33 */
type t =
  | TypeAlias
  | Str
  | Obj
  | Generic
  | Fun;

let decode =
  field("kind", string)
  |> andThen(
       fun
       | "TypeAlias" => (_ => TypeAlias)
       | "Str" => (_ => Str)
       | "Obj" => (_ => Obj)
       | "Generic" => (_ => Generic)
       | "Fun" => (_ => Fun)
       | _ => raise(Failure("Invalid type when decoding Flow json")),
     );