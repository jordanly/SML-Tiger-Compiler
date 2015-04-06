signature TEMP = 
sig
  eqtype temp
  val reset : unit -> unit
  val newtemp : unit -> temp
  structure Table : TABLE sharing type Table.key = temp
  val compare : temp * temp -> order
  val makestring: temp -> string
  type label = Symbol.symbol
  val newlabel : unit -> label
  val namedlabel : string -> label
  structure Set : ORD_SET sharing type Set.Key.ord_key = temp
  structure Map : ORD_MAP sharing type Map.Key.ord_key = temp
end