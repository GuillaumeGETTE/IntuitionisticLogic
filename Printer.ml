open Syntax.SYNTAX
open Formula
open Sequent
module SYMBOLS = struct

let printSeq = "\\vdash"
let printOr = "\\lor"
let printAnd ="\\land"
let printImplies ="\\Rightarrow"
let printTop = "\\top"
let printBot = "\\bot"
let lpar = "("
let rpar = ")"

end

module PRINTER = struct

open SYMBOLS

let rec formula_to_string parenthesis = function
  |Atom(s)-> s
  |Or(f1,f2)->let po = (if parenthesis then lpar else "") in let pf=(if parenthesis then rpar else "") in (po^(formula_to_string true f1))^(((" "^printOr)^" ")^((formula_to_string true f2)^pf))
  |And(f1,f2)->let po = (if parenthesis then lpar else "") in let pf=(if parenthesis then rpar else "") in (po^(formula_to_string true f1))^(((" "^printAnd)^" ")^((formula_to_string true f2)^pf))
  |Implies(f1,f2)->let po = (if parenthesis then lpar else "") in let pf=(if parenthesis then rpar else "") in (po^(formula_to_string true f1))^(((" "^printImplies)^" ")^((formula_to_string true f2)^pf))
  |Top-> printTop |Btm-> printBot
;;

let rec _flts_aux = function
  |[]->""
  |t::q->(" , "^(formula_to_string false t))^(_flts_aux q)
;;

let formula_list_to_string = function
|[]->"";
|t::q->(formula_to_string false t)^(_flts_aux q)
;;

let sequent_to_string ccl  = ((formula_list_to_string (left ccl))^((" "^printSeq)^" "))^(formula_list_to_string ((right ccl)::[]))
;;

let theorem_to_string ccl = sequent_to_string (ccl);;

let print_sequent ccl = (Pervasives.print_string (sequent_to_string ccl); Pervasives.print_newline ());;

let print_theorem ccl = print_sequent (ccl);;
end
