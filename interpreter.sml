datatype Expression =
      VInt of int
    | VBool of bool
    | Id of string
    | AExp of Expression * string * Expression
    | BExp of Expression * string * Expression
    | If of string * Expression * Expression * Expression;

type Function = string * Expression list * Expression
type Application = Function * Expression list 

exception evaluateError of string

fun is_value(e:Expression):bool =
  case e of
    VInt _ => true
  | VBool _ => true
  | Id _ => false
  | AExp(_) => false
  | BExp(_) => false
  | If _ => false

fun toString (e:Expression):string =
      case e of
        VInt i => Int.toString i
      | VBool true => "true"
      | VBool false => "false"
      | AExp(e1,s,e2) => (toString e1)^" "^s^" "^(toString e2)
      | BExp(e1,s,e2) => (toString e1)^" "^s^" "^(toString e2)
      | Id x => x
      | If (s,e1,e2,e3) => "if("^(toString e1)^") then "^(toString e2)^" else "^ (toString e3)

fun print_exp(e:Expression):unit = print (toString e)

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

and substitute(v:Expression list, l:Expression list, e:Expression):Expression =
  (print "> "; print_exp e; print " [Substituicao] \n"; 
   case (v,l,e) of
    (u::nil,k::nil,e) => subst(u,k,e)
  | ((u::us),(k::ks),e) => substitute(us,ks,subst(u,k,e):Expression))

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
          | _ => raise evaluateError("Expressão booleana incorreta.")
      end
  | AExp(e1, s, e2) =>
      let val v1 = evaluateExpression e1
          val v2 = evaluateExpression e2
      in
        case (v1, s, v2) of
          (VInt i1, "+", VInt i2) => VInt(i1+i2)
        | (VInt i1, "-", VInt i2) => VInt(i1-i2)
        | _ => raise evaluateError("Expressão aritmética incorreta.")
      end
  | Id _ => raise evaluateError("Variável livre.") 
  | If ("if",e1,e2,e3) => 
     let val v1 = evaluateExpression e1
         val v2 = evaluateExpression e2
         val v3 = evaluateExpression e3
      in
        case v1 of
            VBool b1 => if b1 then v2 else v3
          | _ => raise evaluateError("Expressão If-then-else incorreta.")
      end

and evaluateExpression(e:Expression):Expression =
  if is_value e then
    e
  else
    (let val v = evalExp(e)
     in
       print "> "; print_exp e; print " [Aplicacao] \n";
       print "> "; print_exp v; print "\n";
       v
     end)

fun apriori(app:Application):Expression =
  case app of
    (("fun",l,exp), vs) => evaluateExpression(substitute(vs, l, exp))
  | _ => raise evaluateError("Aplicação inválida.") 
       
(* TESTES *)
val e1 = (("fun", [Id "x", Id "y"], AExp (Id "x", "+", Id "y")), [VInt 2, VInt 3]);
val e2 = (("fun", [Id "x", Id "y"], AExp (AExp (Id "x", "+", Id "y"),"+", VInt 10)), [VInt 2, VInt 3]);
val e3 = (("fun", [Id "x", Id "y"], BExp (Id "x", "And", Id "y")), [VBool false, VBool true]);
val e4 = (("fun", [Id "x", Id "y"], AExp (Id "x", "+", Id "y")), [AExp (VInt 2, "+", VInt 5), VInt 3]);
val e5 = (("fun", [Id "x", Id "y"], If("if",BExp (Id "x", "Or", Id "y"),VInt 1, VInt 0)), [VBool false, VBool true]);
val e6 = (("fun", [Id "x", Id "y"], If("if",BExp (Id "x", "And", Id "y"),VInt 1, VInt 0)), [VBool false, VBool true]);

