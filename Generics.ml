open Syntax.SYNTAX
open Formula
module GENERICS = struct
   let rec formula_contains_atom q = function
     |Atom(a) when (atom_equal (atom_formula a) q) ->true
     |And(f1,f2) ->(formula_contains_atom q f1)||(formula_contains_atom q f2)
     |Or(f1,f2) ->(formula_contains_atom q f1)||(formula_contains_atom q f2)
     |Implies(f1,f2) ->(formula_contains_atom q f1)||(formula_contains_atom q f2)
     |_->false

let rec formula_list_contains_atom q = function
  |[]->false
  |h::t->(formula_contains_atom q h)||(formula_list_contains_atom q t)

(*let contains_atom seq q =
 (formula_list_contains_atom q (left seq))||(formula_contains_atom q (right seq))*)

let merge l1 l2 = List.rev_append (List.rev l1) l2

let remove x l =
    let rec aux acc = function
    |[]->failwith "Failed executing GENERICS.remove: No such element"
    |t::q when t=x->List.rev_append acc q
    |t::q->aux (t::acc) q
in aux [] l

end
