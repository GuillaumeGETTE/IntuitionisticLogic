module SYNTAX : sig
type a = string
module Formula : sig
type formula = private
      |Top | Btm
      |Atom of a
      |And of formula * formula
      |Or of formula * formula
      |Implies of formula * formula
type t = formula = private 
       |Top | Btm
      |Atom of a
      |And of t * t
      |Or of t * t
      |Implies of t * t

val atom_formula : a -> formula
val and_formula : formula -> formula -> formula
val or_formula : formula -> formula -> formula
val implies_formula : formula -> formula -> formula
val top_formula : unit -> formula
val btm_formula : unit -> formula
val atom_t : a -> formula
val and_t : formula -> formula -> formula
val or_t : formula -> formula -> formula
val implies_t : formula -> formula -> formula
val top_t : unit -> formula
val btm_t : unit -> formula

val new_atom : int Stream.t -> int -> formula
val get_atom_content : formula -> string
val atom_equal : formula -> formula -> bool
end

open Formula
module Sequent : sig
    type sequent
    type t = sequent
    val build_sequent : formula list -> formula -> sequent
    val left : sequent -> formula list
    val right : sequent -> formula
end

module KERNEL : sig
    type provable
    val conclusion : Sequent.sequent -> provable
(*    val provable_from_formula : formula -> provable*)
end

module Rules : sig
    type rule
    val implRule : unit -> rule
    val satRule : unit -> rule
    val build_rule : int -> rule
    (*val apply_impl : formula -> KERNEL.provable -> Forest.ProofTree.tree -> Forest.ProofTree.tree*)
end

end
