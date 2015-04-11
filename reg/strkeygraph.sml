structure StrKeyGraph =
    FuncGraph(
        struct
            type ord_key = string
            val compare = String.compare
        end)