open Syntax.SYNTAX
open Clausification.CLAUSIFICATION
open Generics.GENERICS

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

let provable_from_formula f = 
     match clausification f with
      |Cformula(s,x,q)->conclusion (Sequent.build_sequent (merge s x) q)

end

module Rules = struct
 type rule = string
 let implRule () = "IMPL"
 let implInsertRule () = "\\Rightarrow I"
 let mpRule () = "MP"
 let dblnegRule () = "double negation"
 let satRule () = "Not sure yet, but SAT"
 let build_rule = function
  |0->"IMPL"
  |_->"Dickhoff4"

let get_rule (r: rule) = (r: string)

(*let apply_impl ax seq tr = computeNode (build_rule 0) (axiom (provable_from_formula ax))::tr*)

end
