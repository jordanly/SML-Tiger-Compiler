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
      | leq(ARRAY(_), ARRAY(_)) = false (* TODO *)
      | leq(NIL, NIL) = true
      | leq(NAME(_), NAME(_)) = false (* TODO *)
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