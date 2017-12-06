open SAT_API
open Syntax
open IntuitSolver
open Forest.ProofTree

module Solver = Solve(Syntax)

module DPPL_Interface = struct
  
  let rec bools_to_atom_formulae acc = function
    |[] -> acc
    |(a, b)::tl when b -> bools_to_atom_formulae (a::acc) tl
    |(a, b)::tl -> bools_to_atom_formulae acc tl
  ;;

  let rec formulae_atom_list acc = function
    |[] -> acc
    |Atom(a)::tl -> formulae_atom_list (a::acc) tl
    |_ -> failwith "SafeGuarded unless failure"
  ;;

  let prove s = match (solve s) with
    |SAT(l) -> Answer.YesSAT(bools_to_atom_formulae [] l, "Proved by DPLL")
    |UNSAT(l, b) -> Answer.NoSAT(formulae_atom_list [] l)
  ;;

end

