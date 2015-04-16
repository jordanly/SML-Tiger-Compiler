structure Main = struct

    structure Tr = Translate
    structure TT = Temp.Table
    structure F = MipsFrame
    structure R = RegAlloc

    fun escapeOneVar(i) =
        let
            val boolRefList = FindEscape.getEscapeRefs()
        in
            if i >= List.length(!boolRefList)
            then false
            else

            if !(List.hd(!boolRefList)) = false
            then (List.hd(!boolRefList) := true; true)
            else escapeOneVar(i+1)
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
                val flowgraph : MakeGraph.graphentry StrKeyGraph.graph = MakeGraph.makeFlowgraph instrs
                val (igraph, _, movelist) = Liveness.interferenceGraph flowgraph
                val (alloc, spilled) = RegAlloc.allocateRegisters(igraph, movelist)
                val format0 = Assem.format(fn temp => case TT.look(alloc, temp) of SOME reg => reg | NONE => "NO REGISTER FOUND")
            in 
                (
                    print ("========== Fragment:  " ^ S.name (F.name frame) ^ " ==========\n");
                    print ("=== PRE-CANON " ^ S.name (F.name frame) ^ " ===\n");
                    Printtree.printtree(TextIO.stdOut,body);
                    print ("=== POST-CANON "  ^ S.name (F.name frame) ^ " ===\n");
                    app (fn s => Printtree.printtree(TextIO.stdOut,s)) stms;
                    print ("=== EMIT "  ^ S.name (F.name frame) ^ " ===\n");
                    app (fn i => TextIO.output(TextIO.stdOut, format0 i)) instrs;
                    print ("=== Flowgraph "  ^ S.name (F.name frame) ^ " ===\n");
                    StrKeyGraph.printGraph printGraphNode flowgraph;
                    app (fn i => TextIO.output(out, format0 i)) instrs;
                    if spilled
                    then if escapeOneVar(0) then () else (Err.impossible "Failed to allocate registers")
                    else ();
                    spilled
                )
            end

   fun sortFrags frags =
      let
        val revFrags = rev frags
        fun moveStrings([], newList) = newList
          | moveStrings(a::l, newList) =
              case a of
                  F.STRING(lab,s) => a::moveStrings(l, newList)
                | _ => moveStrings(l, newList)
        fun moveProcs([], newList) = newList
          | moveProcs(a::l, newList) =
              case a of
                  F.PROC{body,frame} => a::moveProcs(l, newList)
                | _ => moveProcs(l, newList)
      in
        moveStrings(frags, moveProcs(frags, []))
      end

    fun compileAbsyn(absyn, filename) = 
        let
            val out = TextIO.openOut (filename ^ ".s")
            val frags : MipsFrame.frag list = sortFrags (Semant.transProg absyn)
            val _ = print "================ AST ==================\n";
            val _ = PrintAbsyn.print(TextIO.stdOut, absyn);
            val _ = print "======== Syntax Errors (if any) ========\n";
            val spilled = foldl (fn(a, b) => b orelse (emitproc out a)) false frags
                            handle e => (TextIO.closeOut out; raise e)
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
            val absyn : Absyn.exp = Parse.parse filename
            val _ = FindEscape.findEscape absyn
        in 
            compileAbsyn(absyn, filename)
        end
end
