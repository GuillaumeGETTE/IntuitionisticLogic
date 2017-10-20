open Kernel

module ProofTree = struct
         type satleaf = string
         type tree = Axiom of KERNEL.provable | Sat of satleaf
                   |Tree of KERNEL.rule * KERNEL.provable * (tree list)
         let computeNode r seq l = Tree(r,seq,l)
         let axiom seq = Axiom(seq)
         let make_satleaf (s:string) = (s:satleaf)
         let sat s = Sat(make_satleaf s)

end
