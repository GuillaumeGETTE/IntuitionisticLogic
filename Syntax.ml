module SYNTAX = struct

type a = string

module Formula = struct

type formula =
      |Top | Btm
      |Atom of a
      |And of formula * formula
      |Or of formula * formula
      |Implies of formula * formula
type t = formula = |Top | Btm
      |Atom of a
      |And of t * t
      |Or of t * t
      |Implies of t * t


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

let atom_t = atom_formula
let and_t = and_formula
let or_t = or_formula
let implies_t =  implies_formula
let top_t = top_formula
let btm_t =  btm_formula

end
open Formula

module Sequent = struct
  type sequent = formula list * formula
  type t = sequent
  let build_sequent l t = (l, t)
  let left = function (l, _) -> l
  let right = function (_, t) -> t
end

module KERNEL = struct
type provable = Sequent.sequent

let conclusion (t: Sequent.sequent) = (t: provable)

end

module Rules = struct
 type rule = string
 let implRule () = "//IMPL"
 let satRule () = "Not sure yet, but SAT"
 let build_rule = function
  |0->"IMPL"
  |_->"Dickhoff4"

let get_rule (r: rule) = (r: string)

(*let apply_impl ax seq tr = computeNode (build_rule 0) (axiom (provable_from_formula ax))::tr*)

end
end
