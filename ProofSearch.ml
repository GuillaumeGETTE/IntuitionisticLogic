module Syntax = struct
type a = char
type t =
  Atom of a | Top | Btm
  | And of t * t | Or of t * t | Implies of t * t
end


module Sequent : sig
  type t
  val sequent : Syntax.t list -> Syntax.t -> t
  val left : t -> Syntax.t list
  val right : t -> Syntax.t
end = struct
  type t = Syntax.t list * Syntax.t
  let sequent l t = (l, t)
  let left = function (l, _) -> l
  let right = function (_, t) -> t
end

module ProofTree : sig

  type satleaf = string
  type tree = Axiom of Sequent.t | Sat of satleaf
            |Tree of Sequent.t * tree list

end = struct
         type satleaf = string
         type tree = Axiom of Sequent.t | Sat of satleaf
                   |Tree of Sequent.t * (tree list)
       end

module Answer = struct
  type sat = YesSAT of Syntax.a list * ProofTree.satleaf | NoSAT of Syntax.a list
  type intuit = YesI of Syntax.a list * ProofTree.tree | NoI of ProofTree.tree * Sequent.t
  type intuitAnswer = True of ProofTree.tree | False of Syntax.t  * Syntax.t
  let yes_i a p = YesI(a, p)
end

module type DPLL_Type = sig
  val prove : Sequent.t -> Answer.sat
end

module D = struct
  let prove s = failwith "Piou"
end

module ProofBuilder (D : DPLL_Type) = struct
  open Answer
  let prove_me_wrong r x q =
    failwith "I'm not sure yet"

  let f ap p s a q = failwith "Not implemented"
  let g h s x a q = failwith "Not implemented"
  let rec intuitProve s x a q opt =
    let seq =  (Sequent.sequent (s @ a) q) in
    let seqx =  (Sequent.sequent ((s @ a) @ x) q) in
    match (D.prove seq) with
    |YesSAT(ap, p) -> (Answer.yes_i ap (f ap p s a q))
    |NoSAT(m) -> (*failwith "still not sure"*)
      match (intuitCheck s x m) with
      |True(h) -> NoI(g h s x a q, seqx)
      |False(c, i) -> failwith "not implemented"
  and intuitCheck s x m = failwith "not implemented"
end

module PB = ProofBuilder(D)
 
