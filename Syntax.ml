module SYNTAX = struct

type a = string

type formula =
      |Top | Btm
      |Atom of a
      |And of formula * formula
      |Or of formula * formula
      |Implies of formula * formula

let atom_formula s = Atom(s)

let and_formula f1 f2 = And(f1,f2)

let or_formula f1 f2 = Or(f1,f2)

let implies_formula f1 f2 = Implies(f1,f2)

let top_formula () = Top
let btm_formula () = Btm

let new_atom s = function
  |1->atom_formula "a"^(string_from_int (Stream.next s))
  |2->atom_formula "b"^(string_from_int (Stream.next s))
  |3->atom_formula "c"^(string_from_int (Stream.next s))
  |_->atom_formula "q"
end
