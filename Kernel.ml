open Syntax.SYNTAX

module KERNEL = struct

type sequent = {left : formula list; right : formula list}

type provable = sequent

let conclusion (t: provable) = (t:sequent)

end


