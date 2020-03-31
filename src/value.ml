open Core

(** Runtime values with source location information.
    Parameterized by underlying type. *)
type 'v t = { source_location : Section.t
            ; datum : 'v }

let to_string { source_location ; datum } f =
  Printf.sprintf "%s:%s" (Section.to_string source_location) (f datum)
