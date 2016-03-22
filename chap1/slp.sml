type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm =
         CompoundStm of stm * stm
       | AssignStm of id * exp
       | PrintStm of exp list

     and exp =
         IdExp of id
       | NumExp of int
       | OpExp of exp * binop * exp
       | EseqExp of stm * exp

val prog =
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

fun maxargs (CompoundStm (e1,e2)) = max (maxargs_exp e1, maxargs_exp e2)
  | maxargs (AssignStm (_,e))     = maxargs_exp e
  | maxargs (PrintStm es)         = max (length es, List.foldl max 0 (map maxargs_exp es))
and maxargs_exp (OpExp (e1,_,e2)) = max (maxargs_exp e1, maxargs_exp e2)
  | maxargs_exp (EseqExp (s,e))   = max (maxargs s, maxargs_exp e)
  | maxargs_exp _                 = 0
and max (x,y)                     = if x < y then y else x



fun interp (s: stm): () =
  let fun interpStm (t, CompoundStm (e1,e2)) =
