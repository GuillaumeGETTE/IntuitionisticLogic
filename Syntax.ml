module SYNTAX = struct

type formula =
      |Top | Btm
      |Atom of String
      |And of formula * formula
      |Or of formula * formula
      |Implies of formula * formula

let atom_formula s = Atom(s)

let and_formula f1 f2 = And(f1,f2)

let or_formula f1 f2 = Or(f1,f2)

let implies_formula f1 f2 = Implies(f1,f2)

let true_formula () = Top
let false_formula () = Btm

end
