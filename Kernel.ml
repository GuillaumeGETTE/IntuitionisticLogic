open Syntax.SYNTAX

module Sequent = struct
  type sequent = formula list * formula
  let build_sequent l t = (l, t)
  let left = function (l, _) -> l
  let right = function (_, t) -> t
end

module KERNEL = struct

type rule = string
type provable = Sequent.sequent

let conclusion (t: Sequent.sequent) = (t: provable)

end


