open Syntax.SYNTAX
open Forest
open Generics.GENERICS
open IKernel

module Answer = struct
  type sat = YesSAT of a list * ProofTree.satleaf | NoSAT of a list
  type intuit = YesI of a list * ProofTree.tree | NoI of a list
  type intuitAnswer = True | False of a list * a * formula * formula list * ProofTree.tree
  let yes_sat l satL = YesSAT(l, satL)
  let no_sat l = NoSAT(l)
  let yes_i a p = YesI(a, p)
  let no_i l = NoI(l)
  let trueA () = True
  let falseA m c i x f = False(m, c, i, x, f)
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

  let rec conjunction = function
    |[]->top_formula ()
    |t::[]->atom_formula t
    |t::q->and_formula (atom_formula t) (conjunction q)
     
  let prove_me_wrong r x q =
    failwith "I'm not sure yet";;
  let f ap p seq = Forest.ProofTree.computeNode (Rules.satRule ()) (KERNEL.conclusion seq) ((Forest.ProofTree.sat p)::[]);;
  let g h s x a q = (Forest.ProofTree.computeNode (Rules.implRule ()) (KERNEL.conclusion (Sequent.build_sequent (merge (merge (merge s (to_formula_list a)) x) (q::[])) (btm_formula ()))) [h]);;
  let rec intuitProve s x a q =
    let seq =  (Sequent.build_sequent (merge s (to_formula_list a)) q) in
    let seqx =  (Sequent.build_sequent (merge (merge s (to_formula_list a)) x) q) in
    match (D.prove seq) with
    |YesSAT(ap, p) -> (Answer.yes_i ap (f ap p seq))
    |NoSAT(m) ->
      match (intuitCheck s x m) with
      |True -> NoI(m)
      |False(m, c, i, xp, f) -> let conjm = conjunction m in let fc = implies_formula conjm (atom_formula c) in
        match (intuitProve (fc::s) x a q) with 
        |YesI(ap, p) -> YesI(ap, (ProofTree.computeNode (Rules.mpRule ()) (KERNEL.conclusion seqx) ((ProofTree.computeNode (Rules.implRule ()) (KERNEL.conclusion (Sequent.build_sequent (merge s xp) fc)) (ProofTree.hypo (merge (merge (merge s xp) (conjm::[])) (i::[]))::f::[]))::p::[])))
        |NoI(m) -> NoI(m)
  and intuitCheck s x m =
    let unpack = function Implies(Implies(Atom(a), Atom(b)), Atom(c)) ->(a, b, c)
                        |_ -> failwith "-- IntuitCheck UNGUARDED exception"
    in
    let rec aux1 l1 l2 j =
      match (unpack j) with 
      |(a, b, c)
           when ((not (List.mem a m))
                 && (not (List.mem b m))
                 && (not (List.mem c m))) ->
              (match (intuitProve s (merge l1 l2) (a::m) (atom_formula b)) with
               |YesI(ap, p) -> raise (Failure(falseA (remove a ap) c j (merge l1 l2) p))
               |NoI(m) ->
                 (match l2 with
                  |[] -> True
                  |a::tl -> (aux1 (a::l1) tl a))
              )
       |(a, b, c)-> match l2 with
                    |[]-> True
                    |a::tl -> aux1 (a::l1) tl a
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
    try (aux1 [] (List.tl x) (List.hd x)) with Failure(a) -> a
  ;;
    (*
    let format_exit l =
      Forest.ProofTree.computeNode "Impl Clause" (Sequent.build_sequent (merge (merge s x) (q::(to_formula_list m))) (btm_formula ())) l in
    try format_exit (aux1 [] [] (List.tl x) (List.hd x)) with Failure(_) -> True(formal_exit []) | Fail(n) -> n
  ;;
     *)
end

module PB = ProofBuilder(D)
