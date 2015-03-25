signature CODEGEN = 
sig
  structure Frame : FRAME
  val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen : CODEGEN =
struct
  structure Frame = MipsFrame
  structure A = Assem
  fun codegen (frame) (stm: Tree.stm) : Assem.instr list =
      let
        val ilist = ref (nil: A.instr list)
        fun emit x = ilist := x:: !ilist
        fun result(gen) =
            let
              val t = Temp.newtemp()
            in
              gen t; t
            end

        fun munchStm() = ()
        and munchExp() = Temp.newtemp()
      in
        rev(!ilist)
      end
end
