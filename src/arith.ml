open Core

module Op = struct
  type t =
    | Add
    | Subtract
    | Mult
    | Div
    | Mod
    | And

end

module Rel = struct
  type t =
    | Equal
end
