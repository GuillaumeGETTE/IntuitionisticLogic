open Syntax.SYNTAX
open Kernel.KERNEL

module SYMBOLS = struct

let printSeq = "\\vdash"
let printOr = "\\lor"
let printAnd ="\\land"
let printImplies ="\\Rightarrow"
let printTop = "\\top"
let printBot = "\\bot"
let printForall = "\\forall"
let printExists = "\\exists"
let lbracket = "\\left["
let rbracket = "\\right]"
let lpar = "("
let rpar = ")"

end

module PRINTER = struct

open SYMBOLS

let rec term_to_string = function
  |Meta(s, _) -> (("Meta(")^(s^")"))
  |Variable(s)-> ("Var("^(s^")"))
  |Constant(s)-> ("Cons("^(s^")"))
  |Operator(s,l)-> let str = term_list_to_string l in (*if (String.equal str "") then s else*) (s^"(")^(str^")")
and  _tlts_aux = function
  |[]->""
  |t::q->(" , "^(term_to_string t))^(_tlts_aux q)
and term_list_to_string = function
     |[]->""
     |t::q->(term_to_string t)^(_tlts_aux q)
;;

let rec formula_to_string parenthesis = function
Predicate(s,l)->let str = term_list_to_string l in if (String.equal str "") then s else (s^"(")^(str^")")
  |Or(f1,f2)->let po = (if parenthesis then lpar else "") in let pf=(if parenthesis then rpar else "") in (po^(formula_to_string true f1))^(((" "^printOr)^" ")^((formula_to_string true f2)^pf))
  |And(f1,f2)->let po = (if parenthesis then lpar else "") in let pf=(if parenthesis then rpar else "") in (po^(formula_to_string true f1))^(((" "^printAnd)^" ")^((formula_to_string true f2)^pf))
  |Implies(f1,f2)->let po = (if parenthesis then lpar else "") in let pf=(if parenthesis then rpar else "") in (po^(formula_to_string true f1))^(((" "^printImplies)^" ")^((formula_to_string true f2)^pf))
  |True-> printTop |False-> printBot
  |Forall(s,f)->((lbracket^printForall)^(s^", "))^((formula_to_string false f)^rbracket)
  |Exists(s,f)->((lbracket^printExists)^(s^", "))^((formula_to_string false f)^rbracket)
;;

let rec _flts_aux = function
  |[]->""
  |t::q->(" , "^(formula_to_string false t))^(_flts_aux q)
;;

let formula_list_to_string = function
|[]->"";
|t::q->(formula_to_string false t)^(_flts_aux q)
;;

let sequent_to_string ccl  = ((formula_list_to_string ccl.left)^((" "^printSeq)^" "))^(formula_list_to_string ccl.right)
;;

let theorem_to_string ccl = sequent_to_string (conclusion ccl);;

let print_sequent ccl = (Pervasives.print_string (sequent_to_string ccl); Pervasives.print_newline ());;

let print_theorem ccl = print_sequent (conclusion ccl);;
end
