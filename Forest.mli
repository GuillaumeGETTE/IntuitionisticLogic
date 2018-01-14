open Syntax.SYNTAX

module ProofTree : sig

   type satleaf = string
   type tree
   val computeNode : Rules.rule -> KERNEL.provable -> tree list -> tree
   val axiom : KERNEL.provable -> tree
   val sat : satleaf -> tree
   val sat_from_string : string -> tree

end
