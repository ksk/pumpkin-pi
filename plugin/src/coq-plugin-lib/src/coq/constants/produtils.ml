(*
 * Utilities for prod types
 *)

open Constr
open Names
open Apputils

let coq_init_data =
  ModPath.MPfile
    (DirPath.make (List.map Id.of_string ["Datatypes"; "Init"; "Coq"]))

(* --- Constants --- *)

(* prod types *)
let prod : types =
  mkInd (MutInd.make1 (Names.KerName.make coq_init_data (Label.make "prod")), 0)

(* Introduction for sigma types *)
let pair : constr =
  mkConstruct (fst (destInd prod), 1)

(* Elimination for sigma types *)
let prod_rect : constr =
  mkConst (Constant.make2 coq_init_data (Label.make "prod_rect"))

(* First projection *)
let fst : constr =
  mkConst (Constant.make2 coq_init_data (Label.make "fst"))

(* Second projection *)
let snd : constr =
  mkConst (Constant.make2 coq_init_data (Label.make "snd"))

(* --- Representations --- *)

(*
 * An application of pair
 *)
type pair_app =
  {
    typ1 : constr;
    typ2 : constr;
    trm1 : constr;
    trm2 : constr;
  }

(*
 * Apply a pair_app
 *)
let apply_pair (app : pair_app) =
  mkAppl (pair, [app.typ1; app.typ2; app.trm1; app.trm2])

(*
 * Deconsruct a sigT type from a type
 *)
let dest_pair (trm : constr) =
  match unfold_args trm with
    [typ1; typ2; trm1; trm2] -> { typ1; typ2; trm1; trm2 }
  | _ -> invalid_arg "dest_pair"
         
(*
 * An application of prod
 *)
type prod_app =
  {
    typ1 : types;
    typ2 : types;
  }

(*
 * Apply a prod_app
 *)
let apply_prod (app : prod_app) : types =
  mkAppl (prod, [app.typ1; app.typ2])

(*
 * Deconstruct a prod
 *)
let dest_prod (trm : types) : prod_app =
  match unfold_args trm with
    [typ1; typ2] -> { typ1; typ2 }
  | _ -> invalid_arg "dest_prod"

(*
 * An application of prod_rect
 *)
type prod_elim =
  {
    to_elim : prod_app;
    p : types;
    proof : constr;
    arg : constr;
  }

(*
 * Eliminate a prod
 *)
let elim_prod (app : prod_elim) =
  let typ1 = app.to_elim.typ1 in
  let typ2 = app.to_elim.typ2 in
  let p = app.p in
  let proof = app.proof in
  let arg = app.arg in
  mkAppl (prod_rect, [typ1; typ2; p; proof; arg])

(*
 * Deconstruct an application of prod
 *)
let dest_prod_elim (trm : constr) =
  match unfold_args trm with
    [typ1; typ2; p; proof; arg] ->
      let to_elim = { typ1; typ2 } in
      { to_elim; p; proof; arg }
  | _ -> invalid_arg "dest_prod_elim"

(*
 * First projection of a prod
 *)
let prod_fst (app : prod_app) trm =
  mkAppl (fst, [app.typ1; app.typ2; trm])

(*
 * Second projection of a prod
 *)
let prod_snd (app : prod_app) trm =
  mkAppl (snd, [app.typ1; app.typ2; trm])

(*
 * Both projections of a prod
 *)
let prod_projections (app : prod_app) trm =
  (prod_fst app trm, prod_snd app trm)
