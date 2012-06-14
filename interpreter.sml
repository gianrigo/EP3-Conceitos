datatype Expression =
      VInt of int
    | VBool of bool
    | Id of string
    | AExp of Expression * string * Expression
    | BExp of Expression * string * Expression
    | If of string * Expression * Expression * Expression;

datatype Application = App of (string * Expression list * Expression) * Expression list 

exception Eval_Error of string

fun is_value(e:Expression):bool =
  case e of
    VInt _ => true
  | VBool _ => true
  | Id _ => false
  | AExp(_) => false
  | BExp(_) => false
  | If _ => false

(* GAMB *)                          
fun comp(e:Expression, s:string) =
  case e of
    Id x => if(x = s) then true else false
   | _ => false

fun subst(v:Expression, x:Expression, e:Expression):Expression =
  case e of
    VInt _ => e
  | VBool _ => e
  | Id y => if comp(x,y) then v else Id y
  | BExp(e1,s,e2) => BExp(subst(v,x,e1), s, subst(v,x,e2))
  | AExp(e1,s,e2) => AExp(subst(v,x,e1), s, subst(v,x,e2))
  | If(s,e1,e2,e3) => If(s,subst(v,x,e1),subst(v,x,e2), subst(v,x,e3))

and substitue(v:Expression list, l:Expression list, e:Expression):Expression =
   case (v,l,e) of
    (u::nil,k::nil,e) => subst(u,k,e)
  | ((u::us),(k::ks),e) => substitue(us,ks,subst(u,k,e):Expression)


fun evalExp(e:Expression):Expression =
  case e of
    VInt v => VInt v
  | VBool v => VBool v
  | BExp(e1, s, e2) =>
      let val v1 = evaluateExpression e1
          val v2 = evaluateExpression e2
      in
        case (v1, s, v2) of
            (VBool b1, "Or", VBool b2) => VBool(b1 orelse b2)
          | (VBool b1, "And", VBool b2) => VBool(b1 andalso b2)
          | (VBool b1, "==", VBool b2) => if (b1 = b2) then VBool(true) else VBool(false)
          | _ => raise Eval_Error("Expressão booleana incorreta.")
      end
  | AExp(e1, s, e2) =>
      let val v1 = evaluateExpression e1
          val v2 = evaluateExpression e2
      in
        case (v1, s, v2) of
          (VInt i1, "+", VInt i2) => VInt(i1+i2)
        | (VInt i1, "-", VInt i2) => VInt(i1-i2)
        | _ => raise Eval_Error("Expressão aritmética incorreta.")
      end
  | Id _ => raise Eval_Error("Variável livre.") 
(*  | If ("if",BExp(e1),e2,e3) => if evaluateExpression(BExp(e1)) then
                             eval e2
                             else eval e3*)

and evaluateExpression(e:Expression):Expression =
  if is_value e then e else
       evalExp(e)

fun evaluateApplication(app:Application):Expression =
  case app of
    App(("fun",l,exp), vs) => evaluateExpression(substitue(vs, l, exp))
  | _ => raise Eval_Error("Aplicação inválida.") 
       
(* TESTES *)
val e1 = App(("fun", [Id "x", Id "y"], AExp (Id "x", "+", Id "y")), [VInt 2, VInt 3]);
val e2 = App(("fun", [Id "x", Id "y"], AExp (AExp (Id "x", "+", Id "y"),"+", VInt 10)), [VInt 2, VInt 3])
val e3 = App(("fun", [Id "x", Id "y"], BExp (Id "x", "And", Id "y")), [VBool false, VBool true]);
val e4 = App(("fun", [Id "x", Id "y"], AExp (Id "x", "+", Id "y")), [AExp (VInt 2, "+", VInt 5), VInt 3]);


