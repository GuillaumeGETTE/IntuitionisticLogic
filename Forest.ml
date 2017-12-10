open Syntax
open IKernel

module ProofTree = struct
         type satleaf = string
         type tree = | Axiom of KERNEL.provable | Sat of satleaf
                   |Tree of Rules.rule * KERNEL.provable * (tree list)
         let computeNode r seq l = Tree(r,seq,l)
         let axiom seq = Axiom(seq)
         let hypo l = KERNEL.conclusion (Sequent.build_sequent l (SYNTAX.top_formula ()))
         let make_satleaf (s:string) = (s:satleaf)
         let sat s = Sat(s)
         let sat_from_string s = sat (make_satleaf s)

end
