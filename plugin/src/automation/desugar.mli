open Environ
open Evd
open Constr

(*
 * Translate each fix or match subterm into a definitionally equal application
 * of an eliminator, returning an updated evar_map. Mutual fix terms and
 * cofix terms are unsupported.
 *)
val desugar_fix_match : env -> evar_map -> constr -> evar_map * constr * types
