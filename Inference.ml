open Syntax
open IKernel
open Forest
open Generics

let apply_mp seq t1 t2 = ProofTree.computeNode (Rules.mpRule ()) (KERNEL.conclusion seq) (t1::t2::[])

let apply_impl seq t1 t2 = ProofTree.computeNode (Rules.implRule ()) (KERNEL.conclusion seq) (t1::t2::[])

(*those primitives computes from some sequents and their proofs a new sequent and its own intuitionistic proof (which contains the sequent)*)
let rec implication = function
  |[]->SYNTAX.top_formula ()
  |t::[]->t
  |t::q-> SYNTAX.implies_formula t (implication q)
        
let unsat_intuit seq t = ProofTree.computeNode (Rules.dblnegRule ()) (KERNEL.conclusion (Sequent.build_sequent ((SYNTAX.implies_formula (Sequent.right seq) (SYNTAX.btm_formula ()))::(Sequent.left seq)) (SYNTAX.btm_formula ()))) (t::[])
                       
let resolve_intuit seq1 t1 seq2 t2 = let r1 = Sequent.right seq1 in let l2 = GENERICS.remove r1 (Sequent.left seq2) in ProofTree.computeNode (Rules.mpRule ()) (KERNEL.conclusion (Sequent.build_sequent (GENERICS.merge (Sequent.left seq1) l2) (Sequent.right seq2))) ((ProofTree.computeNode (Rules.emptyRule ()) (KERNEL.conclusion (Sequent.build_sequent (GENERICS.merge (Sequent.left seq1) l2) (SYNTAX.implies_formula r1 (implication l2)))) (t1::[]))::t2::[])

let currify_intuit seq l t = ProofTree.computeNode (Rules.implInsertRule ()) (KERNEL.conclusion (Sequent.build_sequent (GENERICS.remove l (Sequent.left seq)) (SYNTAX.implies_formula l (SYNTAX.btm_formula())))) (t::[])

