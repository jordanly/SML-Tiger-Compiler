structure MipsFrame : FRAME = 
struct
    datatype access = InFrame of int | InReg of Temp.temp
    type frame = {name: Temp.label, formals: access list}

    (* TODO implement *)
    fun newFrame {name, formals} = 
    let
      (* Assume all escape right now *)
      val frame' = {name=name, formals=[]}
    in
      frame'
    end

    fun name {name=name, formals=formals} = name
    fun formals {name=name, formals=formals} = formals
    
    (* TODO fix for different escapes right now only does true *)
    fun allocLocal frame' escape = 
    let
      fun maxFrameComparator(access', curMax) =
        case access' of
             InFrame x => Int.max(x, curMax)
           | InReg x => curMax
      fun getLastFrameNumber([]) = 0
        | getLastFrameNumber(l) = foldl maxFrameComparator 0 l
    in
      InFrame(getLastFrameNumber(formals frame'))
    end

end
