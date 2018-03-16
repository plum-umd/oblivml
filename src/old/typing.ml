open Syntax

module TEnv =
  struct
    type t = (Type.t option) Var.Map.t

    let option_to_string ele_to_string maybe =
      match maybe with
      | None     -> "*"
      | Some ele -> ele_to_string ele

    let to_string (env : t) = 
      Printf.sprintf "Γ [ %s ]" (String.concat
                                   ", "
                                   (List.map
                                      (fun (var, maybe_typ) -> Printf.sprintf "%s ↦ %s" (Var.to_string var) (option_to_string Type.to_string maybe_typ))
                                      (Var.Map.bindings env)))

    let option_meet (maybe_t1 : Type.t option) (maybe_t2 : Type.t option) : Type.t option =
      match (maybe_t1, maybe_t2) with
      | (None, None)       -> None
      | (None, Some t2)    -> None
      | (Some t1, None)    -> None
      | (Some t1, Some t2) -> assert (t1 = t2); Some t1

    let meet (env1 : t) (env2 : t) : t =
      Var.Map.union (fun x maybe_t1 maybe_t2 -> Some (option_meet maybe_t1 maybe_t2)) env1 env2
  end

module TAlias =
  struct
    type t = Type.t Var.Map.t

    let rec resolve (env : TEnv.t) (alias : t) (typ : Type.t) : Type.t = 
      match typ with
      | Type.TUnit -> Type.TUnit
      | Type.TBit (l, r) -> Type.TBit (l, r)
      | Type.TFlip r -> Type.TFlip r
      | Type.TInt (l, r) -> Type.TInt (l, r)
      | Type.TAlias name -> Var.Map.find name alias
      | Type.TTuple (t1, t2) -> Type.TTuple (resolve env alias t1, resolve env alias t2)
      | Type.TExistential (rs, cs, t') -> Type.TExistential (rs, cs, resolve env alias t')
      | Type.TRecord fields -> Type.TRecord (Field.Map.map (fun t -> resolve env alias t) fields)
      | Type.TArray ele_t -> Type.TArray (resolve env alias ele_t)
      | Type.TFun (arg, ret) -> Type.TFun (resolve env alias arg, resolve env alias ret)
  end

let rec accessible (t : Type.t) : Kind.t =
  match t with
  | Type.TUnit -> Kind.U
  | Type.TBit (_, _) -> Kind.U
  | Type.TFlip _ -> Kind.A
  | Type.TInt (_, _) -> Kind.U
  | Type.TAlias _ -> Kind.U
  | Type.TTuple (t1, t2) ->
     Kind.meet (accessible t1) (accessible t2)
  | Type.TExistential (_, _, t') -> accessible t'
  | Type.TRecord fields ->
     Field.Map.fold (fun _ t k -> Kind.meet (accessible t) k) fields Kind.top
  | Type.TArray _    -> Kind.U
  | Type.TFun (_, _) -> Kind.U

let rec indep_vars scope cs rexp = 
   match rexp with
  | RExp.Bottom -> scope
  | RExp.Var alpha_not -> Scope.filter (fun alpha' -> Constraints.deduce cs (Indep.Indep (RExp.Var alpha_not, RExp.Var alpha'))) scope
  | Rexp.Join (rexp1, rexp2) -> Region.Set.inter (indep_vars scope cs rexp1) (indep_vars scope cs rexp2)
                      
let rec typecheck (scope : Scope.t) (constraints : Constraints.t) (env : TEnv.t) (alias : TAlias.t) (e : expr) : Type.t * TEnv.t =
  match e with
  | EVar x ->
     (try
        let x_t = Var.Map.find x env in
        (match x_t with
         | None   ->
            let err = Printf.sprintf
                        "Attempted to consume a variable which has already been consumed: %s in %s."
                        (Var.to_string x)
                        (expr_to_string e)
            in
            failwith err
         | Some t ->
            (match accessible t with
             | Kind.U ->
                (t, env)
             | Kind.A ->
                (t, Var.Map.add x None env)))
      with
      | Not_found ->
         let err = Printf.sprintf
                     "Variable not found in type environment: %s in %s."
                     (Var.to_string x)
                     (expr_to_string e)
         in
         failwith err)
  | EUnit -> (Type.TUnit, env)
  | EBit (_, l) -> (Type.TBit (l, RExp.bottom), env)
  | EInt (_, l) -> (Type.TInt (l, RExp.bottom), env)
  | EPlus (e1, e2) ->
     let (t1, env') = typecheck scope constraints env alias e1 in
     let (t2, env'') = typecheck scope constraints env' alias e2 in
     (match (t1, t2) with
      | (Type.TInt (l1, r1), Type.TInt (l2, r2)) when Label.equal l1 l2 && RExp.equal r1 r2 ->
         (t1, env'')
      | _ ->
         let err = Printf.sprintf
                     "Adding things which are not numbers, or there is a label/region mismatch: (%s : %s), (%s : %s) in %s"
                     (expr_to_string e1)
                     (Type.to_string t1)
                     (expr_to_string e2)
                     (Type.to_string t2)
                     (expr_to_string e)
         in
         failwith err)
  | EMult (e1, e2) ->
     let (t1, env') = typecheck scope constraints env alias e1 in
     let (t2, env'') = typecheck scope constraints env' alias e2 in
     (match (t1, t2) with
      | (Type.TInt (l1, r1), Type.TInt (l2, r2)) when Label.equal l1 l2 && RExp.equal r1 r2 ->
         (t1, env'')
      | _ ->
         let err = Printf.sprintf
                     "Multiplying things which are not numbers, or there is a label/region mismatch: (%s : %s), (%s : %s) in %s"
                     (expr_to_string e1)
                     (Type.to_string t1)
                     (expr_to_string e2)
                     (Type.to_string t2)
                     (expr_to_string e)
         in
         failwith err)
  | EEq (e1, e2) ->
     let (t1, env') = typecheck scope constraints env alias e1 in
     let (t2, env'') = typecheck scope constraints env' alias e2 in
     (match (t1, t2) with
      | (Type.TInt (l1, r1), Type.TInt (l2, r2)) when Label.equal l1 l2 && RExp.equal r1 r2 ->
         (Type.TBit (l1, r1), env'')
      | _ ->
         let err = Printf.sprintf
                     "Checking equality of things which are not numbers, or there is a label/region mismatch: (%s : %s), (%s : %s) in %s"
                     (expr_to_string e1)
                     (Type.to_string t1)
                     (expr_to_string e2)
                     (Type.to_string t2)
                     (expr_to_string e)
         in
         failwith err)
  | ENot e' ->
     let (t, env') = typecheck scope constraints env alias e' in
     (match t with
      | Type.TBit (l, r) ->
         (t, env')
      | _ ->
         let err = Printf.sprintf
                     "Negation of something which is not a bit: (%s : %s) in %s"
                     (expr_to_string e')
                     (Type.to_string t)
                     (expr_to_string e)
         in
         failwith err)
  | EAnd (e1, e2) ->
     let (t1, env') = typecheck scope constraints env alias e1 in
     let (t2, env'') = typecheck scope constraints env' alias e2 in
     (match (t1, t2) with
      | (Type.TBit (l1, r1), Type.TBit (l2, r2)) when Label.equal l1 l2 && RExp.equal r1 r2 ->
         (Type.TBit (l1, r1), env'')
      | _ ->
         let err = Printf.sprintf
                     "Logical `and` of things which are not bits, or there is a label/region mismatch: (%s : %s), (%s : %s) in %s"
                     (expr_to_string e1)
                     (Type.to_string t1)
                     (expr_to_string e2)
                     (Type.to_string t2)
                     (expr_to_string e)
         in
         failwith err)
  | ETuple (e1, e2) ->
     let (t1, env')  = typecheck scope constraints env alias e1 in
     let (t2, env'') = typecheck scope constraints env' alias e2 in
     (Type.TTuple (t1, t2), env'')
  | ERecord fields ->
     let (fields_types, env') = List.fold_left
                                  (fun (fields_types_acc, env_acc) (field, binding) ->
                                    let (t, env'') = typecheck scope constraints env_acc alias binding in
                                    (Field.Map.add field t fields_types_acc, env''))
                                  (Field.Map.empty, env)
                                  fields
     in
     (Type.TRecord fields_types, env')
  | EArrayInit (size, x, body) ->
     let (size_t, env') = typecheck scope constraints env alias size in
     (match size_t with
      | Type.TInt (l, r) when Label.equal l Label.bottom && RExp.equal r RExp.bottom ->
         let (t_body, env'_plus) = typecheck scope constraints (Var.Map.add x (Some (Type.TInt (Label.bottom, RExp.bottom))) env') alias body in
         (Type.TArray t_body, Var.Map.remove x env'_plus)
      | _ ->
         let err = Printf.sprintf
                     "Array sizes must be public, independent ints: (%s : %s) in %s"
                     (expr_to_string size)
                     (Type.to_string size_t)
                     (expr_to_string e)
         in
         failwith err)
  | ERead (e1, e2)                     ->
     let (e1_t, env') = typecheck scope constraints env alias e1 in
     (match e1_t with
      | Type.TArray ele_t ->
         let (e2_t, env'') = typecheck scope constraints env' alias e2 in
         (match e2_t with
          | Type.TInt (l, r) when Label.equal l Label.bottom && RExp.equal r RExp.bottom ->
             (match accessible ele_t with
              | Kind.U ->
                 (ele_t, env'')
              | Kind.A ->
                 let err = Printf.sprintf
                             "Attempting to read from an array containing affine values without a corresponding wrote: %s array in %s"
                             (Type.to_string ele_t)
                             (expr_to_string e)
                 in
                 failwith err)
          | _ ->
             let err = Printf.sprintf
                         "The array read index must be a public, independent int: (%s : %s) in %s"
                         (expr_to_string e2)
                         (Type.to_string e2_t)
                         (expr_to_string e)
             in
             failwith err)
      | _ ->
         let err = Printf.sprintf
                     "Trying to read from something which does not have array type: (%s : %s) in %s"
                     (expr_to_string e1)
                     (Type.to_string e1_t)
                     (expr_to_string e)
         in
         failwith err)
  | EReadWrite (e1, e2, e3)                ->
     let (e1_t, env') = typecheck scope constraints env alias e1 in
     (match e1_t with
      | Type.TArray ele_t -> 
         let (e2_t, env'') = typecheck scope constraints env' alias e2 in
         (match e2_t with
          | Type.TInt (l, r) when Label.equal l Label.bottom && RExp.equal r RExp.bottom ->
             let (e3_t, env''') = typecheck scope constraints env'' alias e3 in
             if Type.equal e3_t ele_t then
               (ele_t, env''')
             else
               let err = Printf.sprintf
                           "Attempting to write value of wrong type to array: (%s : %s), expected %s array in %s"
                           (expr_to_string e3)
                           (Type.to_string e3_t)
                           (Type.to_string ele_t)
                           (expr_to_string e)
               in
               failwith err
          | _ ->
             let err = Printf.sprintf
                         "The array read index must be a public, independent int: (%s : %s) in %s"
                         (expr_to_string e2)
                         (Type.to_string e2_t)
                         (expr_to_string e)
             in
             failwith err)
      | _ ->
         let err = Printf.sprintf
                     "Trying to read from something with does not have array type: (%s : %s) in %s"
                     (expr_to_string e1)
                     (Type.to_string e1_t)
                     (expr_to_string e)
         in
         failwith err)
  | ELen e' ->
     let (e'_t, env') = typecheck scope constraints env alias e' in
     (match e'_t with
      | Type.TArray ele_t ->
         (Type.TInt (Label.bottom, RExp.bottom), env')
      | _ ->
         let err = Printf.sprintf
                     "Attempting to take length of non-array/vector: (%s : %s) in %s"
                     (expr_to_string e')
                     (Type.to_string e'_t)
                     (expr_to_string e)
         in
         failwith err)
  | EToss ->
     print_endline (Printf.sprintf "%s in %s" (Scope.to_string scope) (expr_to_string e));
     let r = Scope.fresh scope in
     let cs = Scope.fold (fun r' acc -> Constraints.add (Indep.Indep (RExp.Var r, RExp.Var r')) acc) scope Constraints.empty in
     (Type.TExistential ([r], cs, Type.TFlip (RExp.Var r)), env)
  | EUse x ->
     (try
        let x_t = Var.Map.find x env in
        (match x_t with
         | None ->
            let err = Printf.sprintf
                        "Attempted to use a variable which has already been consumed: %s in %s."
                        (Var.to_string x)
                        (expr_to_string e)
            in
            failwith err
         | Some t ->
            (match t with
             | Type.TFlip r ->
                (Type.TBit (Label.top, r), env)
             | _ ->
                let err = Printf.sprintf
                            "Attempting to use a variable which does not have type flip: (%s : %s) in %s"
                            (Var.to_string x)
                            (Type.to_string t)
                            (expr_to_string e)
                in
                failwith err))
      with
      | Not_found ->
         let err = Printf.sprintf
                     "Variable not found in type environment: %s in %s."
                     (Var.to_string x)
                     (expr_to_string e)
         in
         failwith err)
  | EReveal x ->
     (try
        let x_t = Var.Map.find x env in
        (match x_t with
         | None ->
            let err = Printf.sprintf
                        "Attempted to reveal a variable which has already been consumed: %s in %s."
                        (Var.to_string x)
                        (expr_to_string e)
            in
            failwith err
         | Some t ->
            (match t with
             | Type.TFlip r ->
                (Type.TBit (Label.bottom, RExp.bottom), Var.Map.add x None env)
             | _ ->
                let err = Printf.sprintf
                            "Attempting to reveal a variable which does not have type flip: (%s : %s) in %s"
                            (Var.to_string x)
                            (Type.to_string t)
                            (expr_to_string e)
                in
                failwith err))
      with
      | Not_found ->
         let err = Printf.sprintf
                     "Variable not found in type environment: %s in %s."
                     (Var.to_string x)
                     (expr_to_string e)
         in
         failwith err)
  | EMux (k, e1, e2, e3) ->
     (match k with
      | Kind.U ->
         let (e1_t, env') = typecheck scope constraints env alias e1 in
         (match e1_t with
          | Type.TBit (l1, r1) ->
             let (e2_t, env'') = typecheck scope constraints env' alias e2 in
             let (e2_t, env''') = typecheck scope constraints env'' alias e3 in
             let mux_t = Type.join l1 r1 (Type.merge e1_t e2_t) in
             (Type.TTuple (mux_t, mux_t), env''')
          | _ ->
             let err = Printf.sprintf
                         "Mux guard isn't of type bit: (%s : %s) in %s"
                         (expr_to_string e1)
                         (Type.to_string e1_t)
                         (expr_to_string e)
             in
             failwith err)
      | Kind.A ->
         let (e1_t, env') = typecheck scope constraints env alias e1 in
         (match e1_t with
          | Type.TBit (_, r1) ->
             let (e2_t, env'') = typecheck scope constraints env' alias e2 in
             (match e2_t with
              | Type.TFlip r2 ->
                 let (e3_t, env''') = typecheck scope constraints env'' alias e3 in
                 (match e3_t with
                  | Type.TFlip r3 ->
                     if Constraints.deduce constraints (Indep.Indep (r1, r2)) && Constraints.deduce constraints (Indep.Indep (r1, r3)) then
                       let r' = RExp.Join (r2, r3) in
                       let mux_t = Type.TFlip r' in
                       (Type.TTuple (mux_t, mux_t), env''')
                     else
                       let err = Printf.sprintf
                                   "Regions of mux branches are not independent of guard: (%s : %s), (%s : %s), (%s : %s) in %s"
                                   (expr_to_string e1)
                                   (Type.to_string e1_t)
                                   (expr_to_string e2)
                                   (Type.to_string e2_t)
                                   (expr_to_string e3)
                                   (Type.to_string e3_t)
                                   (expr_to_string e)
                       in
                       failwith err
                  | _ ->
                     let err = Printf.sprintf
                                 "Mux else branch isn't type flip: (%s : %s) in %s"
                                 (expr_to_string e3)
                                 (Type.to_string e3_t)
                                 (expr_to_string e)
                     in
                     failwith err)
              | _ ->
                 let err = Printf.sprintf
                             "Mux then branch isn't type flip: (%s : %s) in %s"
                             (expr_to_string e2)
                             (Type.to_string e2_t)
                             (expr_to_string e)
                 in
                 failwith err)
          | _ ->
             let err = Printf.sprintf
                         "Mux guard isn't type bit: (%s : %s) in %s"
                         (expr_to_string e1)
                         (Type.to_string e1_t)
                         (expr_to_string e)
             in
             failwith err))
  | EAlias (name, t, e) ->
     typecheck scope constraints env (Var.Map.add name (TAlias.resolve env alias t) alias) e
  | ELet (pat, e1, e2) ->
     (match pat with
      | XWild  ->
         let (e1_t, env')  = typecheck scope constraints env alias e1 in
         let (e2_t, env'') = typecheck scope constraints env' alias e2 in
         (e2_t, env'')
      | XVar x ->
         let (e1_t, env')       = typecheck scope constraints env alias e1 in
         let (e2_t, env''_plus) = typecheck scope constraints (Var.Map.add x (Some e1_t) env') alias e2 in
         (e2_t, Var.Map.remove x env''_plus)
      | XTuple (x, y) ->
         let (e1_t, env') = typecheck scope constraints env alias e1 in
         (match e1_t with
          | Type.TTuple (t1, t2) ->
             let env'_plus = Var.Map.add y (Some t2) (Var.Map.add x (Some t1) env') in
             let (e2_t, env''_plus) = typecheck scope constraints env'_plus alias e2 in
             let env'' = Var.Map.remove y (Var.Map.remove x env''_plus) in
             (e2_t, env'')
          | _ ->
             let err = "Tuple pattern in let-expr of non-tuple binding"
             in
             failwith err)
      | XUnpack (rs, x) ->
         let (e1_t, env') = typecheck scope constraints env alias e1 in
         (match e1_t with
          | Type.TExistential (rs_e1, cs_e1, e1_t') ->
             let rs_set = Region.Set.of_list rs in
             if Scope.disjoint scope rs_set then
               let rs_exps = List.map (fun alpha -> RExp.Var alpha) rs in
               let (e2_t, env''_plus) = typecheck
                                          (Scope.union scope rs_set)
                                          (Constraints.union constraints (Constraints.rsub cs_e1 rs_e1 rs_exps))
                                          (Var.Map.add x (Some (Type.rsub e1_t' rs_e1 rs_exps)) env)
                                          alias
                                          e2
               in
               let e2_t' = Type.refine e2_t rs_set in
               if Scope.deduce_type scope e2_t' then
                 (e2_t', Var.Map.remove x env''_plus)
               else
                 let err = Printf.sprintf
                             "Scope %s could not deduce type %s in %s"
                             (Scope.to_string scope)
                             (Type.to_string e2_t')
                             (expr_to_string e)
                 in
                 failwith err
             else
               let err = "Binders are not fresh in scope" in
               failwith err
         | _ ->
            let err = "Existential pattern in let-expr of non-existential binding"
            in
            failwith err)
      | XRecord pattern_fields ->
         let (e1_t, env') = typecheck scope constraints env alias e1 in
         (match e1_t with
          | Type.TRecord type_fields ->
             let env'_plus = List.fold_left (fun acc (field, var) -> Var.Map.add var (Some (Field.Map.find field type_fields)) acc) env' pattern_fields in
             let (e2_t, env''_plus) = typecheck scope constraints env'_plus alias e2 in
             let env'' = List.fold_left (fun acc (_, var) -> Var.Map.remove var acc) env''_plus pattern_fields in
             (e2_t, env'')
          | _ ->
             let err = "Record pattern in let-expr of non-record binding"
             in
             failwith err)
      | XFun (name, args, maybe_ret_t) -> 
         (match maybe_ret_t with
          | None ->  (* INS: Currently basically ignoring environment returned by e1... is this a problem? *)
             let body_env = List.fold_left (fun acc (var, typ) -> let typ_resolved = TAlias.resolve env alias typ in 
                                                                  Var.Map.add var (Some typ_resolved) acc) env args in
             let (e1_t, _)  = typecheck scope constraints body_env alias e1 in
             let fun_t = List.fold_right (fun (var, typ) acc -> let typ_resolved = TAlias.resolve env alias typ in Type.TFun (typ_resolved, acc)) args e1_t in
             let (e2_t, env') = typecheck scope constraints (Var.Map.add name (Some fun_t) env) alias e2 in
             (e2_t, (Var.Map.remove name env'))
          | Some ret_t ->
             let body_env = List.fold_left (fun acc (var, typ) -> let typ_resolved = TAlias.resolve env alias typ in Var.Map.add var (Some typ_resolved) acc) env args in
             let fun_t = List.fold_right (fun (var, typ) acc -> 
                             let typ_resolved = TAlias.resolve env alias typ in Type.TFun (typ_resolved, acc)) args (TAlias.resolve env alias ret_t) in
             let (e1_t, _) = typecheck scope constraints (Var.Map.add name (Some fun_t) body_env) alias e1 in
             assert (Type.equal e1_t (TAlias.resolve env alias ret_t));
             let (e2_t, env') = typecheck scope constraints (Var.Map.add name (Some fun_t) env) alias e2 in
             (e2_t, (Var.Map.remove name env'))))
  | ELetRec (defs, rest) ->
     let add_def acc (pat, _) =
       match pat with
       | XFun (name, args, Some ret_t) ->
          let fun_t = List.fold_right (fun (var, typ) acc' ->
                          let typ_resolved = TAlias.resolve env alias typ in Type.TFun (typ_resolved, acc')) args (TAlias.resolve env alias ret_t) in
          Var.Map.add name (Some fun_t) acc
       | _ -> failwith "Mutually recursive definition ill-formed"
     in
     (* Add function types for all mutually recursive defs to environment *)
     let env_with_defs = List.fold_left add_def env defs in
     (* Typecheck all the bodies *)
     let check_body (pat, body) =
       match pat with
       | XFun (name, args, Some ret_t) ->
          let body_env = List.fold_left (fun acc (var, typ) -> let typ_resolved = TAlias.resolve env alias typ in Var.Map.add var (Some typ_resolved) acc) env_with_defs args in
          let (e1_t, _) = typecheck scope constraints body_env alias body in
          assert (Type.equal e1_t (TAlias.resolve env alias ret_t));
          ()
       | _ -> failwith "Mutually recursive definition ill-formed"
     in
     List.iter check_body defs;
     (* Typecheck the rest *)
     let remove_def acc (pat, _) =
       match pat with
       | XFun (name, args, Some ret_t) ->
          Var.Map.remove name acc
       | _ -> failwith "Mutually recursive definition ill-formed"
     in
     let (rest_t, env') = typecheck scope constraints env_with_defs alias rest in
     (rest_t, List.fold_left remove_def env_with_defs defs)
  | EPack (rexps, rs, e', pack_t) ->
     let (e'_t, env') = typecheck scope constraints env alias e' in
     let rs_set = Region.Set.of_list rs in
     let constraints' = List.fold_left2
                          (fun constraints'' alpha rexp ->
                            let alphas = indep_vars scope constraints rexp in
                            let indeps = Region.Set.fold
                                           (fun alpha' indeps' ->
                                             Constraints.add (Indep.Indep (RExp.Var alpha, RExp.Var alpha')) indeps')
                                           alphas
                                           Constraints.empty
                            in
                            Constraints.union constraints'' indeps)
                          Constraints.empty
                          rs
                          rexps
     in
     if Region.Set.disjoint scope rs_set then
       if Constraints.deduces constraints (Constraints.rsub constraints' rs rexps) then
         if Type.equal e'_t (Type.rsub pack_t rs rexps) then
           (Type.TExistential (rs, constraints', pack_t), env')
         else
           failwith "TODO: type of pack body did not match declared pack type."
       else
         failwith "TODO: constraints of pack cannot be deduced by current constraints"
     else
       failwith "TODO: variables to be packed are not fresh"
  | EApp (e1, e2) ->
     let (e1_t, env') = typecheck scope constraints env alias e1 in
     (match e1_t with
      | Type.TFun (t1, t2) ->
         let (e2_t, env'') = typecheck scope constraints env' alias e2 in
         if Type.equal t1 e2_t then
           (t2, env'')
         else
           (print_endline (expr_to_string e1); print_endline (Type.to_string e1_t); print_endline (expr_to_string e2); print_endline (Type.to_string e2_t);
            failwith "Attempting to apply ill-typed argument")
      | _ -> print_endline (Type.to_string e1_t); print_endline (expr_to_string e1); print_endline (expr_to_string e2); failwith "Attempting to apply non-function type")
  | EIf (guard, e1, e2)  ->
     let (guard_t, env') = typecheck scope constraints env alias guard in
     (match guard_t with
      | Type.TBit (l, r) when Label.equal l Label.bottom && RExp.equal r RExp.bottom ->
         let (e1_t, env''_1) = typecheck scope constraints env' alias e1 in
         let (e2_t, env''_2) = typecheck scope constraints env' alias e2 in
         if Type.equal e1_t e2_t then
           (e1_t, TEnv.meet env''_1 env''_2)
         else
           failwith "If branches have different types"
      | _                                                               ->
         failwith "If guard either is not bit type, or has non-bottom label/region")
