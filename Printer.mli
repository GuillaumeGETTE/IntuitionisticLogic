open Syntax.SYNTAX

module SYMBOLS : sig

val printSeq : string
val printOr : string
val printAnd : string
val printImplies : string
val printTop : string
val printBot : string
val printForall : string
val printExists : string
val lbracket : string
val rbracket : string
val lpar : string
val rpar : string

end


module PRINTER : sig

val sequent_to_string : Sequent.sequent -> string
val provable_to_string : KERNEL.provable -> string
val print_sequent : Sequent.sequent -> unit
end
