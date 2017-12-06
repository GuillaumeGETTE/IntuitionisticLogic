open Syntax.SYNTAX
open Forest
open IKernel

module Answer : sig
   type sat = private YesSAT of a list * ProofTree.satleaf | NoSAT of a list
   type intuit = private YesI of a list * ProofTree.tree | NoI of Sequent.sequent * ProofTree.tree
   type intuitAnswer = private True of ProofTree.tree | False of formula  * formula
    val yes_sat : a list -> ProofTree.satleaf -> sat
    val no_sat : a list -> sat
    val yes_i : a list -> ProofTree.tree -> intuit
    val no_i : Sequent.sequent -> ProofTree.tree -> intuit
    val trueA : ProofTree.tree -> intuitAnswer
    val falseA : formula -> formula -> intuitAnswer
end

module type DPLL_Type = sig
  val prove : IKernel.Sequent.sequent->Answer.sat
end

module ProofBuilder (D : DPLL_Type) : sig
   val clause : a list -> a -> a -> formula
   val intuitProve : formula list -> formula list -> a list -> formula -> Answer.intuit
end
