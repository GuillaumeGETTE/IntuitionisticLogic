open Syntax.SYNTAX
open Kernel.KERNEL
open Generics.GENERICS

module CLAUSIFICATION

type flatClause = formula
type implicationClause = formula

let find_new_atom f =
  let rec aux n =
    let s="q"^(Pervasives.string_of_int n) in
    if not(formula_contains_atom s f) then (atom_formula s) else aux (n+1)
  in aux 1

let introduce_atom f = match f with
  |Implies(_,Atom(s))->f
  |_->let q = find_new_atom f in Implies(Implies(f,q),q)


let splitType = |Failure |Success of formula list

let simple_split = function
  |Implies(f1,f2)->

let split f = match f with
  |Implies(_,_)->  (let ls=simple_split f in match ls with
    |Success(l)->ls
    |Failure-> otherSplits f)
  |_->Success((implies_formula)

let rec clausification_process = function
  |[]->[]
  |t::q->match split t with
     |Success(l)->clausification_process (merge l q)
     |Failure->t::(clausification_process q)


end
