open Syntax.SYNTAX

module CLAUSIFICATION : sig
    type clausifiedFormula = formula list * formula list * formula
    val clausification : formula -> clausifiedFormula
end
