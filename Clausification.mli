open Syntax.SYNTAX

module CLAUSIFICATION : sig
    type clausifiedFormula = private Cformula of Formula.formula list * Formula.formula list * Formula.formula
    val clausification : Formula.formula -> clausifiedFormula
    val provable_from_formula : Formula.formula -> KERNEL.provable
end
