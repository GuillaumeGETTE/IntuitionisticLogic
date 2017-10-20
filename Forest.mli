open Kernel

module ProofTree : sig

   type satleaf
   type tree
   val computeNode : KERNEL.rule -> KERNEL.provable -> tree list -> tree
   val axiom : KERNEL.provable -> tree
   val sat : string -> tree

end
