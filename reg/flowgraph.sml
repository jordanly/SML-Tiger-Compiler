structure Flow =
struct
    datatype graphentry = ENTRY of
            {def: Temp.temp list,
             use: Temp.temp list}

  (* Note:  any "use" within the block is assumed to be BEFORE a "def" 
        of the same variable.  If there is a def(x) followed by use(x)
       in the same block, do not mention the use in this data structure,
       mention only the def.

     More generally:
       If there are any nonzero number of defs, mention def(x).
       If there are any nonzero number of uses BEFORE THE FIRST DEF,
           mention use(x).
   *)

end
