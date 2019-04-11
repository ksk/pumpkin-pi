open Constr
open Names
open Globnames
open Coqterms
open Lifting
open Caching
open Search
open Lift
open Desugar
open Unpack
open Utilities
open Pp
open Printer
open Ltac_plugin
open Zooming (* TODO remove when you move coh functionality *)
open Indexing (* TODO same *)
open Environ (* TODO same *)
open Hypotheses (* TODO same *)
open Printing (* TODO same *)
open Debruijn (* TODO same *)
open Hofs (* TODO same *)
open Factoring (* TODO same *)
       
(* --- Options --- *)

(*
 * Prove the coherence property of the algebraic promotion isomorphism
 *)
let opt_search_coh = ref (false)
let _ = Goptions.declare_bool_option {
  Goptions.optdepr = false;
  Goptions.optname = "Generate a proof of coherence in search for DEVOID";
  Goptions.optkey = ["DEVOID"; "search"; "prove"; "coherence"];
  Goptions.optread = (fun () -> !opt_search_coh);
  Goptions.optwrite = (fun b -> opt_search_coh := b);
}

let is_search_coh () = !opt_search_coh

(*
 * Prove section and retraction
 *)
let opt_search_equiv = ref (false)
let _ = Goptions.declare_bool_option {
  Goptions.optdepr = false;
  Goptions.optname = "Generate proof of equivalence in search for DEVOID";
  Goptions.optkey = ["DEVOID"; "search"; "prove"; "equivalence"];
  Goptions.optread = (fun () -> !opt_search_equiv);
  Goptions.optwrite = (fun b -> opt_search_equiv := b);
}

let is_search_equiv () = !opt_search_equiv

(* --- Commands --- *)

(* TODO refactor below, comment, fill in *)
(* TODO test on other types besides list/vect in file *)
let prove_coherence env evd orn =
  let env_coh = zoom_env zoom_lambda_term env orn.promote in
  let a = mkRel 1 in
  let is = on_type unfold_args env_coh evd a in
  let b_sig = mkAppl (orn.promote, snoc a is) in
  let b_sig_typ = reduce_type env_coh evd b_sig in
  let ib = mkAppl (orn.indexer, snoc a is) in
  let ib_typ = reduce_type env_coh evd ib in
  let packer = (dest_sigT b_sig_typ).packer in
  let proj_ib = project_index { index_type = ib_typ; packer} b_sig in
  let coh = reconstruct_lambda env_coh (mkAppl (eq_refl, [ib_typ; proj_ib])) in
  let coh_typ = reconstruct_product env_coh (mkAppl (eq, [ib_typ; ib; proj_ib])) in
  (coh, coh_typ)

(*
 * TODO move, explain, refactor common w/ refolding in search/lifting
 * mention c_body is the reduced body of a constructor, and env_c_b is the env
 *)
let get_rec_args typ env_c_b evd c_body =
  List.filter (on_type (is_or_applies typ) env_c_b evd) (unfold_args c_body)

(*
 * TODO move, explain
 *)
let section_eq_lemmas env evd a_typ =
  let ((i, i_index), u) = destInd a_typ in
  Array.mapi
    (fun c_index _ ->
      let c = mkConstructU (((i, i_index), c_index + 1), u) in
      let (env_c_b, c_body) = zoom_lambda_term env (expand_eta env evd c) in
      let c_body = reduce_term env_c_b c_body in
      let c_body_typ = reduce_type env_c_b evd c_body in
      let refl = mkAppl (eq_refl, [c_body_typ; c_body]) in
      let recs = get_rec_args a_typ env_c_b evd c_body in
      let env_lemma, off =
        List.fold_right
          (fun r (e, off) ->
            let r1 = shift_by off r in (* original rec arg *)
            let r_t = reduce_type e evd r in (* rec arg type *)
            let e_r = push_local (Anonymous, r_t) e in (* e with new rec arg *)
            let r1 = shift r in (* shifted original rec arg *)
            let r2 = mkRel 1 in (* new rec arg *)
            let r_t  = shift r_t in (* new rec arg type *)
            let r_eq = apply_eq {at_type = r_t; trm1 = r1; trm2 = r2} in
            (push_local (Anonymous, r_eq) e_r, off + 2))
          recs
          (env_c_b, 0)
      in
      let (body, _, _) =
        List.fold_right
          (fun _ (b, h_eq, c_app) ->
            let h_eq_r = destRel h_eq in
            let (_, _, h_eq_t) = CRD.to_tuple @@ lookup_rel h_eq_r env_lemma in
            let app = dest_eq (shift h_eq_t) in
            let typ = app.at_type in
            let r1 = app.trm1 in
            let r2 = app.trm2 in
            let abs_inner = all_eq_substs (shift r1, mkRel 1) (shift c_app) in
            let new_inner = all_eq_substs (shift r1, shift r2) (shift c_app) in
            let eq_ind_rel = mkLambda (Anonymous, typ, mkAppl (eq, [shift typ; shift c_app; abs_inner])) in
            (mkAppl (eq_ind, [typ; r1; eq_ind_rel; shift (shift b); r2; h_eq]), shift (shift h_eq), new_inner))
          recs
          (refl, mkRel 1, shift_by off c_body)
      in reconstruct_lambda env_lemma body)
    (* TODO what happens for trees when there are multiple IHs? What does the body look like? *)
    ((lookup_mind i env).mind_packets.(i_index)).mind_consnames

(* TODO refactor below, comment, fill in *)
(* TODO clean up too *)
(* TODO test on other types besides list/vect in file *)
let prove_section promote_n forget_n env evd orn =
  let env_sec = zoom_env zoom_lambda_term env orn.promote in
  let a = mkRel 1 in
  let a_typ = first_fun (reduce_type env_sec evd a) in
  let ((i, i_index), u) = destInd a_typ in
  let mutind_body = lookup_mind i env in
  (* compose eq lemmas now *)
  let elim = type_eliminator env_sec (i, i_index) in
  let npm = mutind_body.mind_nparams in
  (* TODO what about env_sec? *)
  let eq_typ = reduce_type env_sec evd (mkRel 1) in (* TODO prob redundant *)
  let typ_args = unfold_args eq_typ in
  let nargs = new_rels env_sec npm in
  let p =
    shift_by
      nargs (* TODO why? what is this exactly? same in search *)
      (reconstruct_lambda_n
         env_sec
         (mkAppl
            (eq,
             [eq_typ;
              mkAppl
                (make_constant forget_n, (* TODO can be global env *)
                (snoc
                   (mkAppl
                      (make_constant promote_n, (* TODO can be global env *)
                      (snoc (mkRel 1) typ_args)))
                   typ_args));
              mkRel 1
             ]))
         npm)
  in
  let (env_pms, elim_typ) = zoom_n_prod env npm (infer_type env evd elim) in
  let (n, p_t, b) = destProd elim_typ in
  let env_p = push_local (n, p_t) env_pms in
  let pms = shift_all_by nargs (mk_n_rels npm) in (* TODO why nargs? *)
  let eq_lemmas = section_eq_lemmas env evd a_typ in
  let section_case c_i c =
    let rec case e pms p_rel p args c =
      match kind c with
      | App (_, _) ->
         (* conclusion: apply eq lemma and beta-reduce *)
         let pms_and_args = List.append pms args in
         let eq_lemma = eq_lemmas.(c_i) in
         reduce_term e (mkAppl (eq_lemmas.(c_i), pms_and_args))
      | Prod (n, t, b) ->
         let case_b = case (push_local (n, t) e) (shift_all pms) (shift p_rel) (shift p) in
         if applies p_rel t then
           (* IH *)
           let t' = reduce_term e (mkAppl (p, unfold_args t)) in
           (* TODO build args in reverse order w cons; reverse later *)
           let _ :: sec_a :: a :: _ = unfold_args t' in (* TODO wrap eq to get each arg like we do for sigT and so on; same for eq_refl and eq_ind *)
           let args_b = snoc (mkRel 1) (shift_all (snoc a (snoc sec_a (List.tl (List.rev args))))) in
           mkLambda (n, t', case_b args_b b)
         else
           (* Product *)
           mkLambda (n, t, case_b (snoc (mkRel 1) (shift_all args)) b)
      | _ ->
         failwith "unexpected case"
    in
    case env_p pms (mkRel 1) p [] c
  in
  let cs = List.mapi section_case (take_except nargs (factor_product b)) in
  let app =
       apply_eliminator
         {
           elim;
           pms;
           p;
           cs;
           final_args = mk_n_rels nargs;
         }
  in reconstruct_lambda env_sec app
                        
(*
 * Identify an algebraic ornament between two types
 * Define the components of the corresponding equivalence
 * (Don't prove section and retraction)
 *)
let find_ornament n_o d_old d_new =
  let (evd, env) = Pfedit.get_current_context () in
  let trm_o = unwrap_definition env (intern env evd d_old) in
  let trm_n = unwrap_definition env (intern env evd d_new) in
  match map_tuple kind (trm_o, trm_n) with
  | Ind ((m_o, _), _), Ind ((m_n, _), _) ->
    let (_, _, lab_o) = KerName.repr (MutInd.canonical m_o) in
    let (_, _, lab_n) = KerName.repr (MutInd.canonical m_n) in
    let name_o = Label.to_id lab_o in
    let name_n = Label.to_string lab_n in
    let auto_n = with_suffix (with_suffix name_o "to") name_n in
    let n = Option.default auto_n n_o in
    let idx_n = with_suffix n "index" in
    let orn = search_orn_inductive env evd idx_n trm_o trm_n in
    ignore (define_term idx_n evd orn.indexer true);
    (* TODO move all of these into msg_notice *)
    Printf.printf "Defined indexer %s\n\n" (Id.to_string idx_n);
    let promote = define_term n evd orn.promote true in
    Printf.printf "Defined promotion %s\n\n" (Id.to_string n);
    let inv_n = with_suffix n "inv" in
    let forget = define_term inv_n evd orn.forget true in
    Printf.printf "Defined forgetful function %s\n\n" (Id.to_string inv_n);
    (if is_search_coh () then (* TODO move these out, too *)
       let env = Global.env () in
       let coh, coh_typ = prove_coherence env evd orn in
       let coh_n = with_suffix n "coh" in
       let _ = define_term ~typ:coh_typ coh_n evd coh true in
       Printf.printf "Defined coherence proof %s\n\n" (Id.to_string coh_n)
     else
       ());
    (if is_search_equiv () then
       let env = Global.env () in
       (* TODO can we use promote/forget above instead of names? *)
       let section = prove_section n inv_n env evd orn in
       let sec_n = with_suffix n "section" in
       let _ = define_term sec_n evd section true in
       Printf.printf "Defined section proof %s\n\n" (Id.to_string sec_n)
       (* TODO also do retraction *)
     else
       ());
    (try
       save_ornament (trm_o, trm_n) (promote, forget)
     with _ ->
       Printf.printf "WARNING: Failed to cache ornamental promotion.")
  |_ ->
    failwith "Only inductive types are supported"

(*
 * Lift a definition according to a lifting configuration, defining the lifted
 * definition and declaring it as a lifting of the original definition.
 *)
let lift_definition_by_ornament env evd n l c_old =
  let lifted = do_lift_defn env evd l c_old in
  ignore (define_term n evd lifted true);
  try
    let old_gref = global_of_constr c_old in
    let new_gref = ConstRef (Lib.make_kn n |> Constant.make1) in
    declare_lifted old_gref new_gref;
  with _ ->
    Printf.printf "WARNING: Failed to cache lifting."

(*
 * Lift an inductive type according to a lifting configuration, defining the
 * new lifted version and declaring type-to-type, constructor-to-constructor,
 * and eliminator-to-eliminator liftings.
 *)
let lift_inductive_by_ornament env evm n s l c_old =
  let ind, _ = destInd c_old in
  let ind' = do_lift_ind env evm n s l ind in
  let env' = Global.env () in
  Feedback.msg_notice (str "Defined lifted inductive type " ++ pr_inductive env' ind')

(*
 * Lift the supplied definition or inductive type along the supplied ornament
 * Define the lifted version
 *)
let lift_by_ornament ?(suffix=false) n d_orn d_orn_inv d_old =
  let (evd, env) = Pfedit.get_current_context () in
  let c_orn = intern env evd d_orn in
  let c_orn_inv = intern env evd d_orn_inv in
  let c_old = intern env evd d_old in
  let n_new = if suffix then suffix_term_name c_old n else n in
  let s = if suffix then Id.to_string n else "_" ^ Id.to_string n in
  let us = map_tuple (unwrap_definition env) (c_orn, c_orn_inv) in
  let are_inds = isInd (fst us) && isInd (snd us) in
  let lookup os = map_tuple Universes.constr_of_global (lookup_ornament os) in
  let (c_from, c_to) = if are_inds then lookup us else (c_orn, c_orn_inv) in
  let l = initialize_lifting env evd c_from c_to in
  let u_old = unwrap_definition env c_old in
  if isInd u_old then
    let from_typ = fst (on_type ind_of_promotion_type env evd l.orn.promote) in
    if not (equal u_old from_typ) then
      lift_inductive_by_ornament env evd n_new s l c_old
    else
      lift_definition_by_ornament env evd n_new l c_old
  else
    lift_definition_by_ornament env evd n_new l c_old

(*
 * Translate each fix or match subterm into an equivalent application of an
 * eliminator, defining the new term with the given name.
 *
 * Mutual fix or cofix subterms are not supported.
 *)
let do_desugar_constant ident const_ref =
  ignore
    begin
      qualid_of_reference const_ref |> Nametab.locate_constant |>
      Global.lookup_constant |> transform_constant ident desugar_constr
    end

(*
 * Translate fix and match expressions into eliminations, as in
 * do_desugar_constant, compositionally throughout a whole module.
 *
 * The optional argument is a list of constants outside the module to include
 * in the translated module as if they were components in the input module.
 *)
let do_desugar_module ?(incl=[]) ident mod_ref =
  let open Util in
  let consts = List.map (qualid_of_reference %> Nametab.locate_constant) incl in
  let include_constant subst const =
    let ident = Label.to_id (Constant.label const) in
    let tr_constr env evm = subst_globals subst %> desugar_constr env evm in
    let const' =
      Global.lookup_constant const |> transform_constant ident tr_constr
    in
    Globmap.add (ConstRef const) (ConstRef const') subst
  in
  let init () = List.fold_left include_constant Globmap.empty consts in
  ignore
    begin
      qualid_of_reference mod_ref |> Nametab.locate_module |>
      Global.lookup_module |> transform_module_structure ~init ident desugar_constr
    end

(*
 * Unpack sigma types in the functional signature of a constant.
 *
 * This transformation assumes that the input constant was generated by
 * ornamental lifting.
 *)
let do_unpack_constant ident const_ref =
  let env = Global.env () in
  let evm = ref (Evd.from_env env) in
  let term =
    qualid_of_reference const_ref |> Nametab.locate_constant |>
    unpack_constant env evm
  in
  ignore (define_term ident !evm term true)
