open Syntax.SYNTAX
open Generics.GENERICS

module CLAUSIFICATION = struct

type clausifiedFormula = Cformula of formula list * formula list * formula
type splitType = |Failure |Success of formula list

let streamA = Stream.from (fun i -> Some (i+1))
let streamB = Stream.from (fun i -> Some (i+1))
let streamC = Stream.from (fun i -> Some (i+1))
let streamQ = Stream.from (fun i -> Some (i+1))
let streamP = Stream.from (fun i -> Some (i+1))

let rename_all_atoms f =
  let hasht = Hashtbl.create 10 in
  let rec aux fAux = match fAux with
  |Atom(_)->let s=get_atom_content fAux in (try Hashtbl.find hasht s with |Not_found->let q = (new_atom streamP 0) in (Hashtbl.add hasht s q; q))
  |And(f1,f2)->and_formula (aux f1) (aux f2)
  |Or(f1,f2)->or_formula (aux f1) (aux f2)
  |Implies(f1,f2)->implies_formula (aux f1) (aux f2)
  |_->fAux
in aux f

let introduce_atom f = let f1 = rename_all_atoms f in
match f1 with
  |Implies(_,Atom(s))->f1
  |_->let q = new_atom streamQ 4 in implies_formula (implies_formula f1 q) q


let simple_split = function
  |Implies(Or (a,b),Atom(s))-> let q = atom_formula s in Success((implies_formula a q)::(implies_formula b q)::[])
  |Implies(Atom(s),And(a,b))->let q = atom_formula s in Success((implies_formula q a)::(implies_formula q b)::[])
  |Implies(a,Implies(b,c))->Success((implies_formula (and_formula a b) c)::[])
  |_->Failure

let clause_split a b c =
  match a with
  |Atom(qa)->(match b with |Atom(qb)->(match c with |Atom(qc)->Failure
                                                    |_->let q = new_atom streamC 3 in Success((implies_formula (implies_formula a b) q)::(implies_formula q c)::[]))
                           |_->let q = new_atom streamB 2 in Success((implies_formula (implies_formula a q) c)::(implies_formula b q)::[]))
  |_->let q = new_atom streamA 1 in Success((implies_formula (implies_formula q b) c)::(implies_formula q a)::[])

let other_splits = function
  |Implies(a,Or(Atom(b1),Atom(b2)))->Failure
  |Implies(a,Or(b1,(Atom(q))))->let b = new_atom streamB 2 in Success((implies_formula a (or_formula b (atom_formula q)))::(implies_formula b b1)::[])
  |Implies(a,Or(Atom(q),b2))->let b = new_atom streamB 2 in Success((implies_formula a (or_formula (atom_formula q) b))::(implies_formula b b2)::[])
  |Implies(a,Or(b1,b2))->let b = new_atom streamB 2 in Success((implies_formula a (or_formula b b2))::(implies_formula b b1)::[])
  |Implies(And(Atom(a1),Atom(a2)),b)->Failure
  |Implies(And(a1,Atom(q)),b)->let a = new_atom streamA 1 in Success((implies_formula (and_formula a (atom_formula q)) b)::(implies_formula a a1)::[])
  |Implies(And(Atom(q),a2),b)->let a = new_atom streamA 1 in Success((implies_formula (and_formula (atom_formula q) a) b)::(implies_formula a a2)::[])
  |Implies(And(a1,a2),b)->let a = new_atom streamA 1 in Success((implies_formula (and_formula a a2) b)::(implies_formula a a1)::[])
  |Implies(Implies(a,b),c)->clause_split a b c
  |_->Failure

let split f = match f with
  |Implies(_,_)->  (let ls=simple_split f in match ls with
    |Success(l)->ls
    |Failure-> other_splits f)
  |_->Success((implies_formula (top_formula ()) f)::[])

let rec clausification_process = function
  |[]->[]
  |t::q->match split t with
     |Success(l)->clausification_process (merge l q)
     |Failure->t::(clausification_process q)

let clausification f =
  let rec aux l1 l2 = function
    |[]->l1,l2
    |t::q->(match t with
      |Implies(Implies(a,b),c)->aux l1 (t::l2) q
      |_->aux (t::l1) l2 q)
    in match (introduce_atom f) with
    |Implies(b,q)->let l1,l2=aux [] [] (clausification_process (b::[])) in Cformula(l1,l2,q)
    |_->failwith "code impossible"

end
