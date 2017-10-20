open Syntax.SYNTAX

module Sequent : sig
    type sequent
    val build_sequent : formula list -> formula -> sequent
    val left : sequent -> formula list
    val right : sequent -> formula
end

module KERNEL : sig
    type rule
    type provable
    val conclusion : Sequent.sequent -> provable
end
