open Syntax.SYNTAX
open Forest
open Generics.GENERICS
open Kernel

module Answer = struct
  type sat = YesSAT of a list * ProofTree.satleaf | NoSAT of a list
  type intuit = YesI of a list * ProofTree.tree | NoI of Sequent.sequent * ProofTree.tree
  type intuitAnswer = True of ProofTree.tree | False of formula  * formula
  let yes_sat l satL = YesSAT(l, satL)
  let no_sat l = NoSAT(l)
  let yes_i a p = YesI(a, p)
  let no_i seq p = NoI(seq, p)
  let trueA p = True(p)
  let falseA f1 f2 = False(f1, f2)
end

module type DPLL_Type = sig
  val prove : Sequent.sequent -> Answer.sat
end

module D = struct
  let prove s = failwith "Piou"
end

module ProofBuilder (D : DPLL_Type) = struct
  open Answer
  exception Failure of Answer.intuitAnswer
  let to_formula_list l =
    let rec aux acc = function a::tl -> aux ((atom_formula a)::acc) tl | [] -> List.rev acc in
    aux [] l;;

  let prove_me_wrong r x q =
    failwith "I'm not sure yet";;
  let format_no_intuit_anwser = failwith "not implemented";;
  let h_build = failwith  "Not Implemented";;
  let clause ap a c = failwith "Not implemented";;
  let format_intuit_answer a = failwith "not implemented";;
  let f ap p seq = Forest.ProofTree.computeNode (Rules.satRule ()) (KERNEL.conclusion seq) ((Forest.ProofTree.sat p)::[]);;
  let g h s x a q = (Forest.ProofTree.computeNode (Rules.implRule ()) (KERNEL.conclusion (Sequent.build_sequent (merge (merge (merge s (to_formula_list a)) x) (q::[])) (btm_formula ()))) [h]);;
  let rec intuitProve s x a q =
    let seq =  (Sequent.build_sequent (merge s (to_formula_list a)) q) in
    let seqx =  (Sequent.build_sequent (merge (merge s (to_formula_list a)) x) q) in
    match (D.prove seq) with
    |YesSAT(ap, p) -> (Answer.yes_i ap (f ap p seq))
    |NoSAT(m) ->
      match (intuitCheck s x m) with
      |True(h) -> NoI(seqx, g h s x a q)
      |False(c, i) ->
        match (intuitProve (c::s) (remove i x) a q) with
        |YesI(ap, p) -> YesI(ap, (ProofTree.computeNode (Rules.implRule ()) (KERNEL.conclusion seqx) ((Forest.ProofTree.axiom (KERNEL.provable_from_formula i))::p::[])))
        |NoI(cp, p) -> NoI(cp, (ProofTree.computeNode (Rules.implRule ()) (KERNEL.conclusion seqx) ((Forest.ProofTree.axiom (KERNEL.provable_from_formula i))::p::[])))
  and intuitCheck s x m =
    let unpack = function Implies(Implies(Atom(a), Atom(b)), Atom(c)) ->(a, b, c)
                        |_ -> failwith "-- IntuitCheck UNGUARDED exception"
    in
    let rec aux1 acc l1 l2 j =
      match (unpack j) with 
      |(a, b, c)
           when ((not (List.mem a m))
                 && (not (List.mem b m))
                 && (not (List.mem c m))) ->
              (match (intuitProve s (merge l1 l2) (a::m) (atom_formula b)) with
               |YesI(ap, p) -> raise (Failure(falseA (clause ap a c) (implies_formula (implies_formula (atom_formula a) (atom_formula b)) (atom_formula c))))
               |NoI(m, p) ->
                 (match l2 with
                  |[] -> ((p)::acc)
                  |a::tl -> (aux1 (p::acc) (a::l1) tl a))
              )
       |(a, b, c)-> match l2 with
                    |[]-> acc
                    |a::tl -> aux1 acc (a::l1) tl a
    in
(*
    let rec iter_on_x acc l1 j = match j with (Implies(Implies(Atom(a), Atom(b)), Atom(c)))::l2
                                 when ((not (List.mem a m))
                                       && (not (List.mem b m))
                                       && (not (List.mem c m)))->
                                     (match (intuitProve s (merge l1 l2) (a::m) (atom_formula b)) with
                                      |YesI(ap, p) -> raise (Failure(falseA (clause ap a c) (implies_formula (implies_formula (atom_formula a) (atom_formula b)) (atom_formula c))))
                                      |NoI(m, p) -> 
                                        match l2 with 
                                          a::tl -> (iter_on_x ((p, b)::acc) (j::l1) l2 a)
                                         |_ -> acc
                                     )
                                    |[] -> acc
                                    |_ -> failwith "unguarded exception -- Intuitcheck"
    in
*)
    try True(format_intuit_answer (aux1 [] [] (List.tl x) (List.hd x))) with Failure(a) -> a
  ;;
    (*
    let format_exit l =
      Forest.ProofTree.computeNode "Impl Clause" (Sequent.build_sequent (merge (merge s x) (q::(to_formula_list m))) (btm_formula ())) l in
    try format_exit (aux1 [] [] (List.tl x) (List.hd x)) with Failure(_) -> True(formal_exit []) | Fail(n) -> n
  ;;
     *)
end

module PB = ProofBuilder(D)
