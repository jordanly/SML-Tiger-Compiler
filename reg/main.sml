structure Main = struct

    structure Tr = Translate
    structure F = MipsFrame
    (* structure R = RegAlloc *)

    fun emitproc out (F.PROC{body,frame}) =
            let val _ = print ("========== Fragment:  " ^ S.name (F.name frame) ^ " ==========\n")
                val _ = print ("=== PRE-CANON " ^ S.name (F.name frame) ^ " ===\n")
                val _ = Printtree.printtree(TextIO.stdOut,body);
                val stms : Tree.stm list = Canon.linearize body
                val _ = print ("=== POST-CANON "  ^ S.name (F.name frame) ^ " ===\n")
                val _ = app (fn s => Printtree.printtree(TextIO.stdOut,s)) stms;
                val _ = print ("=== EMIT "  ^ S.name (F.name frame) ^ " ===\n")
                val format0 = Assem.format(F.makestring)
                val stms' : Tree.stm list = Canon.traceSchedule(Canon.basicBlocks stms)
                val instrs : Assem.instr list =   List.concat(map (MipsGen.codegen frame) stms')
                val _ = app (fn i => TextIO.output(TextIO.stdOut,format0 i)) instrs

                val _ = print ("=== Flowgraph "  ^ S.name (F.name frame) ^ " ===\n")
                val flowgraph : MakeGraph.graphentry StrKeyGraph.graph = MakeGraph.makeFlowgraph instrs
                fun printGraphNode (id, node as {def, use, ismove}) =
                    id ^ "(def: " ^ (foldl (fn (temp, str) => str ^ Temp.makestring temp ^ ", ") "" def)
                    ^ " -- use: " ^ (foldl (fn (temp, str) => str ^ Temp.makestring temp ^ ", ") "" use)
                    ^ " -- ismove: " ^ (Bool.toString ismove) ^ ")"
                val _ = StrKeyGraph.printGraph printGraphNode flowgraph
            in 
                app (fn i => TextIO.output(out,format0 i)) instrs
            end
      | emitproc out (F.STRING(lab,s)) =
            (
                print ("========== Fragment:  " ^ (S.name lab) ^ " ==========\n");
                TextIO.output(TextIO.stdOut, F.string(lab,s));
                TextIO.output(out, F.string(lab,s))
            )

   fun withOpenFile fname f = 
        let
            val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out) 
            handle e => (TextIO.closeOut out; raise e)
        end 

   fun compile filename = 
        let
            val absyn : Absyn.exp = Parse.parse filename
            val _ = print "======== Syntax Errors (if any) ========\n";
            val frags : MipsFrame.frag list= (FindEscape.findEscape absyn; Semant.transProg absyn)
        in 
            withOpenFile (filename ^ ".s") 
            (fn out => (app (emitproc out) frags))
       end
end
