open Syntax.SYNTAX
open Forest
open Kernel

module Answer : sig
    type sat
    type intuit
    type intuitAnswer
    val yes_i : a list -> ProofTree.tree -> intuit
end

module type DPLL_Type = sig
  val prove : Sequent.sequent->Answer.sat
end

module ProofBuilder (D : DPLL_Type) : sig
   val intuitProve : formula list -> formula list -> formula list -> formula -> Answer.intuit
end
