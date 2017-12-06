


module ProofBuilder (D : DPLL_Type) = struct
  
(*
        D.prove returns :
     =>   YesSAT(ap, p) where ap is the subset of A used to prove the sequent, and p the proof
     =>   NoSAT(m) where m is a model, disproving the sequent 
*)




  let intuitProve  s x a q =
    (* s : flat_clauses,
       x : implication clauses,
       a : Assumptions,
       q : atom, goal
       intuitProve returns the proof of (S, X, A |- q), if it is provable


     *)
    let seq = Sequent.build_sequent (merge s (to_formula_list a) q) in
    (*seq : S, A |- q *)

    match (D.prove seq) with
    |YesSAT(ap, p) -> 
      let proof = f ap p seq in
      YesI(ap, proof)
      (*the sequent is true, syntaxically, we don't need the "intuitionnistic theory"
        we just rewrite the sequent into the proof
       *)
    |NoSAT(m) -> 
      (*If the sequent cannot be proved because there is a model m that would dispove it, 
        we have to check if m is compatible with the implication clauses in x *)
      match (intuitCheck s x m) with
      |True(h) -> NoI(seqx, (*missing here*))
        (*the model is coherent with the set X, and we successfully disproved the sequent, we've got a proof of m and m disprove seq*)
      (*work to do here*)
      |False(c, i) -> 
        (*the model is not compatible with X, and we're going to loop with an extra-assumption *)

