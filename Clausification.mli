open Syntax.SYNTAX

module CLAUSIFICATION : sig
    type clausifiedFormula = private Cformula of formula list * formula list * formula
    val clausification : formula -> clausifiedFormula
end
