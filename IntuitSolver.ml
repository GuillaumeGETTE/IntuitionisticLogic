open Syntax.SYNTAX
open Forest
open Generics.GENERICS
open Kernel

module Answer = struct
  type sat = YesSAT of a list * ProofTree.satleaf | NoSAT of a list
  type intuit = YesI of a list * ProofTree.tree | NoI of ProofTree.tree * Sequent.sequent
  type intuitAnswer = True of ProofTree.tree | False of formula  * formula
  let yes_i a p = YesI(a, p)
end

module type DPLL_Type = sig
  val prove : Sequent.sequent -> Answer.sat
end

module D = struct
  let prove s = failwith "Piou"
end

module ProofBuilder (D : DPLL_Type) = struct
  open Answer
  let prove_me_wrong r x q =
    failwith "I'm not sure yet"

  let f ap p s a q = failwith "Not implemented"
  let g h s x a q = failwith "Not implemented"
  let rec intuitProve s x a q =
    let seq =  (Sequent.build_sequent (merge s a) q) in
    let seqx =  (Sequent.build_sequent (merge (merge s a) x) q) in
    match (D.prove seq) with
    |YesSAT(ap, p) -> (Answer.yes_i ap (f ap p s a q))
    |NoSAT(m) -> (*failwith "still not sure"*)
      match (intuitCheck s x m) with
      |True(h) -> NoI(g h s x a q, seqx)
      |False(c, i) -> failwith "not implemented"
  and intuitCheck s x m = failwith "not implemented"
end

module PB = ProofBuilder(D)
