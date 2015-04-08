structure Main = struct

    structure Tr = Translate
    structure F = MipsFrame
    (* structure R = RegAlloc *)

    fun emitproc out (F.PROC{body,frame}) =
            let val _ = print ("========== Fragment:  " ^ S.name (F.name frame) ^ " ==========\n")
                val _ = print ("=== PRE-CANON " ^ S.name (F.name frame) ^ " ===\n")
                val _ = Printtree.printtree(TextIO.stdOut,body);
                val stms = Canon.linearize body
                val _ = print ("=== POST-CANON "  ^ S.name (F.name frame) ^ " ===\n")
                val _ = app (fn s => Printtree.printtree(TextIO.stdOut,s)) stms;
                val _ = print ("=== EMIT "  ^ S.name (F.name frame) ^ " ===\n")
                val format0 = Assem.format(F.makestring)
                val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
                val instrs =   List.concat(map (MipsGen.codegen frame) stms')
                val _ = app (fn i => TextIO.output(TextIO.stdOut,format0 i)) instrs

                val _ = print ("=== Flowgraph "  ^ S.name (F.name frame) ^ " ===\n")
                val flowgraph = MakeGraph.makeFlowgraph instrs
                fun printGraphNode (id, node) = id
                val _ = FlowGraph.printGraph printGraphNode flowgraph
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
            val absyn = Parse.parse filename
            val _ = print "======== Syntax Errors (if any) ========\n";
            val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
        in 
            withOpenFile (filename ^ ".s") 
            (fn out => (app (emitproc out) frags))
       end
end
