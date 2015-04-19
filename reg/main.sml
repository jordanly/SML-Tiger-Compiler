structure Main = struct

    structure Tr = Translate
    structure TT = Temp.Table
    structure F = MipsFrame
    structure R = RegAlloc

    fun printBoolList(boolListRef) =
      (app (fn x : bool ref => if !x then print "true, " else print "false, ") boolListRef; print "\n")

    fun escapeOneVar(i) =
        let
            val boolRefList = FindEscape.getEscapeRefs()
        in
            if i >= List.length(!boolRefList)
            then false
            else

            if !(List.nth(!boolRefList, i)) = false
            then (List.nth(!boolRefList, i) := true; true)
            else escapeOneVar(i+1)
        end

    fun getMaxNumArgs(fragList : F.frag list) = 
        let
          fun lenFormals(frag : F.frag) = 
            case frag of
                  F.PROC {body, frame} => List.length(F.formals frame)
                | F.STRING(_, _) => 0
          fun iterator(frag : F.frag, curMax) = 
            case frag of
                F.PROC {body=b, frame=f} => if curMax < List.length(F.formals f)
                                            then lenFormals frag
                                            else curMax
              | F.STRING(_, _) => curMax
        in
          foldl iterator 0 (fragList)
        end

    fun emitproc out (F.STRING(lab,s)) =
            (
                print ("========== Fragment:  " ^ (S.name lab) ^ " ==========\n");
                TextIO.output(TextIO.stdOut, F.string(lab,s));
                TextIO.output(out, F.string(lab,s));
                false
            )
      | emitproc out (F.PROC{body,frame}) =
            let 
                fun printGraphNode (id, node as {def, use, ismove}) =
                    id ^ "(def: " ^ (foldl (fn (temp, str) => str ^ Temp.makestring temp ^ ", ") "" def)
                    ^ " -- use: " ^ (foldl (fn (temp, str) => str ^ Temp.makestring temp ^ ", ") "" use)
                    ^ " -- ismove: " ^ (Bool.toString ismove) ^ ")"

                val stms : Tree.stm list = Canon.linearize body
                val stms' : Tree.stm list = Canon.traceSchedule(Canon.basicBlocks stms)
                val instrs : Assem.instr list = List.concat(map (MipsGen.codegen frame) stms')
                val instrs' : Assem.instr list = #body (F.procEntryExit3(frame, instrs, getMaxNumArgs(Translate.getResult())))
                val flowgraph : MakeGraph.graphentry StrKeyGraph.graph = MakeGraph.makeFlowgraph instrs'
                val (igraph, _, movelist) = Liveness.interferenceGraph flowgraph
                val (alloc, spilled) = RegAlloc.allocateRegisters(igraph, movelist)
                val format0 = Assem.format(fn temp => case TT.look(alloc, temp) of SOME reg => reg | NONE => "NO REGISTER FOUND")
                val format1 = Assem.format(fn temp => Temp.makestring temp)
            in 
                (
                    print ("========== Fragment:  " ^ S.name (F.name frame) ^ " ==========\n");
                    print ("=== PRE-CANON " ^ S.name (F.name frame) ^ " ===\n");
                    Printtree.printtree(TextIO.stdOut,body);
                    print ("=== POST-CANON "  ^ S.name (F.name frame) ^ " ===\n");
                    app (fn s => Printtree.printtree(TextIO.stdOut,s)) stms;
                    print ("=== EMIT "  ^ S.name (F.name frame) ^ " ===\n");
                    app (fn i => TextIO.output(TextIO.stdOut, format0 i)) instrs';
                    (*
                    print ("=== Flowgraph "  ^ S.name (F.name frame) ^ " ===\n");
                    StrKeyGraph.printGraph printGraphNode flowgraph;
                    *)
                    (*
                    print ("=== Interference Graph "  ^ S.name (F.name frame) ^ " ===\n");
                    Liveness.show(TextIO.stdOut, igraph);
                    *)
                    print ("=== Register allocation "  ^ S.name (F.name frame) ^ " ===\n");
                    R.printAlloc(alloc, map (TempKeyGraph.getNodeID) (TempKeyGraph.nodes igraph));
                    app (fn i => TextIO.output(out, format0 i)) instrs';
                    if spilled
                    then if escapeOneVar(0) then () else (Err.impossible "Failed to allocate registers")
                    else ();
                    spilled
                )
            end

   fun writeRuntime(outstream) = 
      let
        val runtimeStream = TextIO.openIn "runtimele.s"
        val _ = TextIO.output(outstream, TextIO.inputAll runtimeStream)
        val _ = TextIO.closeIn runtimeStream
      in
        ()
      end

   fun writeSysspim(outstream) = 
      let
        val runtimeStream = TextIO.openIn "sysspim.s"
        val _ = TextIO.output(outstream, TextIO.inputAll runtimeStream)
        val _ = TextIO.closeIn runtimeStream
      in
        ()
      end

    fun compileAbsyn(absyn, filename) = 
        let
            fun isStringFrag (F.STRING _) = true | isStringFrag _ = false
            fun isProcFrag (F.PROC _) = true | isProcFrag _ = false

            val out = TextIO.openOut (filename ^ ".s")
            val _ = Translate.resetFragList() (* reset since new tree *)
            val frags : MipsFrame.frag list = Semant.transProg absyn
            val stringFrags : MipsFrame.frag list = List.filter isStringFrag frags
            val procFrags : MipsFrame.frag list = List.filter isProcFrag frags
            val _ = print "================ AST ==================\n";
            val _ = PrintAbsyn.print(TextIO.stdOut, absyn);
            (* Emit .data segment *)
            val _ = TextIO.output(out, ".data\n")
            val _ = foldl (fn(a, b) => b orelse (emitproc out a)) false stringFrags
                            handle e => (TextIO.closeOut out; raise e)
            (* Emit .text segment *)
            val _ = TextIO.output(out, "\n.text\n")
            (* Write runtime before program*)
            val _ = writeRuntime out
            (* Write program *)
            val spilled = foldl (fn(a, b) => b orelse (emitproc out a)) false procFrags
                            handle e => (TextIO.closeOut out; raise e)
            (* Write sysspim at end *)
            val _ = writeSysspim out
            val _ = TextIO.closeOut out
        in 
            (
                if spilled
                then compileAbsyn(absyn, filename)
                else ()
            )
        end
   
   fun compile filename = 
        let
            val _ = print "======== Syntax Errors (if any) ========\n";
            val absyn : Absyn.exp = Parse.parse filename
            val _ = FindEscape.findEscape absyn
        in 
            compileAbsyn(absyn, filename)
        end
end
