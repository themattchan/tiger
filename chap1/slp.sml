type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm
  = CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

and exp
    = IdExp of id
    | NumExp of int
    | OpExp of exp * binop * exp
    | EseqExp of stm * exp

val prog =
    CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
                CompoundStm(AssignStm("b",
                                      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
                                              OpExp(NumExp 10, Times, IdExp"a"))),
                            PrintStm[IdExp "b"]))

fun maxargs (CompoundStm (s1,s2)) = max (maxargs s1, maxargs s2)
  | maxargs (AssignStm (_,e))     = maxargs_exp e
  | maxargs (PrintStm es)         = max (length es, List.foldl max 0 (map maxargs_exp es))
and maxargs_exp (OpExp (e1,_,e2)) = max (maxargs_exp e1, maxargs_exp e2)
  | maxargs_exp (EseqExp (s,e))   = max (maxargs s, maxargs_exp e)
  | maxargs_exp _                 = 0


type table = (id * int) list
exception UnboundVariableError

fun lookup (nil,_)        = raise UnboundVariableError
  | lookup ((k,v)::t', x) = if x = k then v else lookup (t',x)

fun update(t,k,v) = (k,v)::t


fun interp (s: stm) =
  (* interpStm : stm * table -> table *)
  let fun interpStm (CompoundStm (s1,s2),t)
        = interpStm (s2, interpStm (s1,t))
        | interpStm (AssignStm (k,e),t)
          = let val (n,t1) = interpExp (e,t)
            in update (t, k, n)
            end
        | interpStm (PrintStm es, t)
          = let fun print1 (e,t) =
                  let val (n, t1) = interpExp (e,t)
                  in print (Int.toString n);
                     print " ";
                     t1
                  end
                val tx = List.foldr print1 t es in
                print "\n"; tx
            end
      (* interpExp : exp * table -> int * table *)
      and interpExp (IdExp i, t)
          = (lookup (t,i), t)
        | interpExp (NumExp n, t)
          = (n,t)
        | interpExp (OpExp (e1,bop,e2),t)
          = let val (n1,t1) = interpExp (e1,t)
                val (n2,t2) = interpExp (e2,t)
            in ((interpBop bop) (n1,n2), t2)
            end
        | interpExp (EseqExp (s,e),t)
          = let val t1 = interpStm (s,t)
            in interpExp (e,t1)
            end
      and interpBop Plus  = (op + )
        | interpBop Minus = (op - )
        | interpBop Times = (op * )
        | interpBop Div   = (op div )
  in interpStm (s,[])
  end
