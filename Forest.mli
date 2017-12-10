open Syntax
open IKernel

module ProofTree : sig

   type satleaf
   type tree
   val computeNode : Rules.rule -> KERNEL.provable -> tree list -> tree
   val axiom : KERNEL.provable -> tree
   val hypo : SYNTAX.formula list -> tree
   val sat : satleaf -> tree
   val sat_from_string : string -> tree

end
