module SYNTAX : sig
type a
type formula = private
      |Top | Btm
      |Atom of a
      |And of formula * formula
      |Or of formula * formula
      |Implies of formula * formula

val atom_formula : a -> formula
val and_formula : formula -> formula -> formula
val or_formula : formula -> formula -> formula
val implies_formula : formula -> formula -> formula
val top_formula : unit -> formula
val btm_formula : unit -> formula

val new_atom : int Stream.t -> int -> formula
val atom_equal : formula -> formula -> bool
end
