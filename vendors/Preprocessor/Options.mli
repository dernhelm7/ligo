(* CLI options *)

module type S =
  sig
    val input   : string option (* input file            *)
    val dirs    : string list   (* -I                    *)
    val show_pp : bool          (* --show-pp             *)
    val offsets : bool          (* negation of --columns *)
  end

include S