open Core
open Stdio

module Un =
  struct
    module Op =
      struct
        type t = unit
      end
    module Rel =
      struct
        type t = unit
      end
  end

module Bin =
  struct
    module Op =
      struct
        type t =
          | Add
          | Subtract
          | Mult
          | And
      end
    module Rel =
      struct
        type t =
          | Equal
      end
  end
