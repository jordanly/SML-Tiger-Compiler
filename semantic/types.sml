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
      | leq(ARRAY(ty1, unique1), ARRAY(ty2, unique2)) = (unique1 = unique2)
      | leq(NIL, NIL) = true
      | leq(NAME(sym1, _), NAME(sym2, _)) = String.compare(Symbol.name sym1, Symbol.name sym2) = EQUAL) (* TODO is this correct? *)
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

end
