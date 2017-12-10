open Syntax
open IKernel
open Forest

val apply_mp : Sequent.sequent -> ProofTree.tree -> ProofTree.tree -> ProofTree.tree

val apply_impl : Sequent.sequent -> ProofTree.tree -> ProofTree.tree -> ProofTree.tree

val unsat_intuit : Sequent.sequent -> ProofTree.tree -> ProofTree.tree

val resolve_intuit : Sequent.sequent -> ProofTree.tree -> Sequent.sequent -> ProofTree.tree -> ProofTree.tree

val currify_intuit : Sequent.sequent -> SYNTAX.formula -> ProofTree.tree -> ProofTree.tree
