module GENERICS = struct
   let rec formula_contains_atom (Atom(s) as q) = function
     |Atom(a) when (String.equals a q) ->true
     |And(f1,f2) (formula_contains_atom q f1)||(formula_contains_atom q f2)
     |Or(f1,f2) (formula_contains_atom q f1)||(formula_contains_atom q f2)
     |Implies(f1,f2) (formula_contains_atom q f1)||(formula_contains_atom q f2)
     |_->false

let rec formula_list_contains_atom q = function
  |[]->false
  |h::t->(formula_contains_atom q h)||(formula_list_contains_atom q t)

let contains_atom seq q =
  (formula_list_contains q seq.left)||(formula_list_contains q seq.right)

let merge l1 l2 =
 let rec aux =function
  |[]->l2
  |t::q-> t::(aux q)
 in aux l1


end
