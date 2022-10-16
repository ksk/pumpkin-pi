(*
 * Utilities for types
 *)

open Environ
open Evd
open Constr
open Reducers
                                                      
(* --- Higher-order functions --- *)

(*
 * Apply a function on a type instead of on the term
 *)
val on_type :
  (env -> evar_map -> types -> 'a) -> (* the function to apply *)
  env ->
  evar_map ->
  types -> (* the type *)
  'a

(* Like on_type, but reduce the type using the supplied reducer first *)
val on_red_type :
  reducer ->
  (env -> evar_map -> types -> 'a) ->
  env ->
  evar_map ->
  types ->
  'a

(* Like on_red_type, but use the default reducer *)
val on_red_type_default :
  (env -> evar_map -> types -> 'a) ->
  env ->
  evar_map ->
  types ->
  'a

(* Returns a list of all subterms of a term with a given type. *)
val subterms_with_type :
  env ->
  evar_map ->
  types ->
  constr ->
  (evar_map * (env * constr)) list
