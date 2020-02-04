open Core

type 'a t =
  | Outcome of 'a
  | Choice of 'a t * 'a t

let pi1 a =
  match a with
  | Outcome _ -> a
  | Choice (l, _) -> l

let pi2 a =
  match a with
  | Outcome _ -> a
  | Choice (_, r) -> r

let negate (x : Bool.t t) : (Bool.t t) = failwith "TODO"

let logand (x : Bool.t t) (y : Bool.t t) = failwith "TODO"

let add (x : Int.t t) (y : Int.t t) = failwith "TODO"

let sub (x : Int.t t) (y : Int.t t) = failwith "TODO"

let mult x y = failwith "TODO"

let div x y = failwith "TODO"

let modulus x y = failwith "TODO"

let bitand x y = failwith "TODO"

let inteq x y = failwith "TODO"

let cond g v1 v2 = failwith "TODO"

let rec bit n =
  match n with
  | 0 -> Choice (Outcome true, Outcome false)
  | n ->
    let reccase = bit (n - 1) in
    Choice (reccase, reccase)

let rec bind (a : 'a t) ~(f : 'a -> 'b t) : 'b t =
  match a with
  | Outcome a -> f a
  | Choice (l, r) ->
    let l' = bind l (fun x -> pi1 (f x)) in
    let r' = bind r (fun x -> pi2 (f x)) in
    Choice (l', r')

let return (x : 'a) : 'a t = Outcome x

let map = `Define_using_bind
