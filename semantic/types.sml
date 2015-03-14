structure Types =
struct

	type unique = unit ref

	datatype ty = 
            RECORD of (unit -> (Symbol.symbol * Symbol.symbol) list) * unique (* (fieldname, typeid) *)
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
          | NAME of Symbol.symbol * ty option ref
          | UNIT
          | BOTTOM

  	datatype comp = 
  		    LT
  		  | GT
  		  | EQ
  		  | INCOMP (* incomparable *)

    fun leq(BOTTOM, _) = true
      | leq(_, UNIT) = true
      | leq(NIL, RECORD(_)) = true
      | leq(INT, INT) = true
      | leq(STRING, STRING) = true
      | leq(RECORD(_, unique1), RECORD(_, unique2)) = (unique1 = unique2)
      | leq(ARRAY(_, unique1), ARRAY(_, unique2)) = (unique1 = unique2)
      | leq(NIL, NIL) = true
      | leq(NAME(sym1, _), NAME(sym2, _)) = String.compare(Symbol.name sym1, Symbol.name sym2) = EQUAL (* TODO is this correct? *)
      | leq(_, _) = false

    fun comp(t1, t2) = 
    	if leq(t1, t2) andalso leq(t2, t1)
    		then EQ
    	else if leq(t1, t2)
    		then LT
    	else if leq(t2, t1)
    		then GT
    	else
    		INCOMP

    fun eq(t1, t2) = 
        comp(t1, t2) = EQ

    fun printTy ty =
      case ty of
           RECORD(_, _) => print "type is record\n"
         | NIL => print "type is nil\n"
         | INT => print "type is int\n"
         | STRING => print "type is string\n"
         | ARRAY(arrTy, _) => (print "array: "; printTy ty)
         | NAME(sym, _) => print ("name type is " ^ Symbol.name sym ^ "\n")
         | UNIT => print "type is unit\n"
         | BOTTOM => print "type is bottom\n"

end
