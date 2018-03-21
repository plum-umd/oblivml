module Set = struct
  module type S =
    sig
      include Set.S
      val disjoint : t -> t -> bool
    end

  module Make (Ord : Set.OrderedType) : S with type elt = Ord.t =
    struct
      include Set.Make(Ord)
      let disjoint s1 s2 = equal (inter s1 s2) empty
    end
end
