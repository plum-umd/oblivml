type t = O | I

let to_string b = 
  match b with
    | O -> "O"
    | I -> "I"
