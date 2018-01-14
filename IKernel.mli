open Syntax.SYNTAX

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
end

module Rules : sig
    type rule
    val implRule : unit -> rule
    val satRule : unit -> rule
    val build_rule : int -> rule
    (*val apply_impl : formula -> KERNEL.provable -> Forest.ProofTree.tree -> Forest.ProofTree.tree*)
end
