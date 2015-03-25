signature CODEGEN = 
sig
  structure Frame : FRAME
  val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen : CODEGEN =
struct
  structure Frame = MipsFrame
  fun codegen _ _ = []
end
