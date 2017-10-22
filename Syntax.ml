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
  |1->atom_formula ("a"^(string_of_int (Stream.next s)))
  |2->atom_formula ("b"^(string_of_int (Stream.next s)))
  |3->atom_formula ("c"^(string_of_int (Stream.next s)))
  |4->atom_formula "q"
  |_->atom_formula ("p"^(string_of_int (Stream.next s)))

let get_atom_content = function |Atom(s)->(s:string) |_-> failwith "Not an atom"

let atom_equal a1 a2 = (String.equal (get_atom_content a1) (get_atom_content a2))

end
