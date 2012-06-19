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

fun isValue(e:Expression):bool =
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
      | AExp(e1,s,e2) => "( "^(toString e1)^" "^s^" "^(toString e2)^" )"
      | BExp(e1,s,e2) => "( "^(toString e1)^" "^s^" "^(toString e2)^" )"
      | Id x => x
      | If (s,e1,e2,e3) => "if("^(toString e1)^") then "^(toString e2)^" else "^ (toString e3)

fun printExp(e:Expression):unit = print (toString e)

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
   case (v,l,e) of
     (nil,nil,e) =>  e
   | ((u::us),(k::ks),e) =>
       (print "> "; printExp e; print " [";printExp (hd(l));print " <- ";printExp (hd(v));print "] \n";
        substitute(us,ks,(subst(u,k,e))))

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
  if isValue e then e
  else
    (print "> "; printExp e; print "\n";(*Fica repetitivo, mas pelo menos não parece que pula etapas da avaliação.*)
     let val v = evalExp(e)
     in
       print "> "; printExp e; print " [Aplicacao] \n";
       print "> "; printExp v; print "\n";
       v
     end)

fun evaluateArgs(exp:Expression list):Expression list =
    case (exp) of
      (k::nil) => evaluateExpression(k)::nil
    | (k::ks) => evaluateExpression(k)::evaluateArgs(ks)


fun apriori(app:Application):Expression =
  case app of
    (("fun",l,exp), vs) => evaluateExpression(substitute(evaluateArgs(vs), l, exp))
  | _ => raise evaluateError("Aplicação inválida.")

fun evalExpLazy(e:Expression):Expression =
  case e of
    VInt v => VInt v
  | VBool v => VBool v
  | BExp(e1, s, e2) =>
      let val v1 = evaluateExpressionLazy e1
      in
        case (v1, s) of
          (VBool b1, "Or") => if (b1 = true) then VBool(true)
                            else let val v2 = evaluateExpressionLazy e2
			           in
			             case v2 of
			 	       (VBool b2) => VBool (b2) (*Aqui (b1=false), então se (b2=true) => (BExp=true),
				   					           senao (BExp=false).*)
 			             | _ => raise evaluateError("Expressão booleana incorreta.")
				       end
        | (VBool b1, "And") => if (b1 = false) then VBool(false)
 			       else let val v2 = evaluateExpressionLazy e2
				    in
	 			      case v2 of
				        (VBool b2) => VBool (b2) (*Aqui (b1=true), então se (b2=true) => (BExp=true),
					        			           senao (BExp=false).*)
				      | _ => raise evaluateError("Expressão booleana incorreta.")
				        end
        | (VBool b1, "==") => let val v2 = evaluateExpressionLazy e2
		              in
			        case v2 of
			          (VBool b2) => if (b1 = b2) then VBool(true) else VBool(false)
			        | _ => raise evaluateError("Expressão booleana incorreta.")
			      end
        | _ => raise evaluateError("Expressão booleana incorreta.")
      end
  | AExp(e1, s, e2) => (*Ainda precisa mudar para lazy.*)
      let val v1 = evaluateExpressionLazy e1
          val v2 = evaluateExpressionLazy e2
      in
        case (e1, s, e2) of
          (VInt i1, "+", VInt i2) => VInt(i1+i2)
        | (VInt i1, "-", VInt i2) => VInt(i1-i2)
      (*| (AExp i1, "+", VInt i2) => VInt((evaluateExpressionLazy i1)+i2)*)
        | _ => raise evaluateError("Expressão aritmética incorreta.")
      end
  | Id _ => raise evaluateError("Variável livre.") 
  | If ("if",e1,e2,e3) => 
      let val v1 = evaluateExpressionLazy e1
      in
        case v1 of
          VBool b1 => if b1 then (evaluateExpressionLazy e2) else (evaluateExpressionLazy e3)
        | _ => raise evaluateError("Expressão If-then-else incorreta.")
      end

and evaluateExpressionLazy(e:Expression):Expression =
  if isValue e then e
  else
     (print "> "; printExp e; print "\n";(*Fica repetitivo, mas pelo menos não parece que pula etapas da avaliação.*)
     let val v = evalExpLazy(e)
     in
       print "> "; printExp e; print " [Aplicacao] \n";
       print "> "; printExp v; print "\n";
       v
     end)

(*Precisa ainda fazer a aplicação após cada substituição.*)
fun substAndApply(v:Expression, x:Expression, e:Expression):Expression =
  case e of
    VInt _ => e
  | VBool _ => e
  | Id y => if comp(x,y) then v else Id y
  (*Nos abaixo, é necessário separar caso a caso e aplicar a substuição+aplicação de acordo.
    Exemplo: Na expressão binária, separar os casos em que uma das variáveis é booleana
             e aplicar o lazy evaluation sobre isso.*)
  | BExp(e1,s,e2) => BExp(substAndApply(v,x,e1), s, substAndApply(v,x,e2))
  | AExp(e1,s,e2) => AExp(substAndApply(v,x,e1), s, substAndApply(v,x,e2))
  | If(s,e1,e2,e3) => If(s,substAndApply(v,x,e1),substAndApply(v,x,e2), substAndApply(v,x,e3))

and substituteAndApply(v:Expression list, l:Expression list, e:Expression):Expression =
   case (v,l,e) of
     (nil,nil,e) =>  e
   | ((u::us),(k::ks),e) => 
       (print "> "; printExp e; print " [";printExp (hd(l));print " <- ";printExp (hd(v));print "] \n";
        substituteAndApply(us,ks,(substAndApply(u,k,e))))

fun evaluateArgsLazy(exp:Expression list):Expression list =
    case (exp) of
      (k::nil) => evaluateExpressionLazy(k)::nil
    | (k::ks) => evaluateExpressionLazy(k)::evaluateArgs(ks)

fun sobdemanda(app:Application):Expression =
  case app of
    (("fun",l,exp), vs) => evaluateExpressionLazy(substituteAndApply(evaluateArgsLazy(vs), l, exp))
  | _ => raise evaluateError("Aplicação inválida.")

(* TESTES *)
val e1 = (("fun", [Id "x", Id "y"], AExp (Id "x", "+", Id "y")), [VInt 2, VInt 3]);
val e2 = (("fun", [Id "x", Id "y"], AExp (AExp (Id "x", "+", Id "y"),"+", VInt 10)), [VInt 2, VInt 3]);
val e3 = (("fun", [Id "x", Id "y"], BExp (Id "x", "And", Id "y")), [VBool false, VBool true]);
val e4 = (("fun", [Id "x", Id "y"], AExp (Id "x", "+", Id "y")), [AExp (VInt 2, "+", VInt 5), VInt 3]);
val e5 = (("fun", [Id "x", Id "y"], If("if",BExp (Id "x", "Or", Id "y"),VInt 1, VInt 0)), [VBool false, VBool true]);
val e6 = (("fun", [Id "x", Id "y"], If("if",BExp (Id "x", "And", Id "y"),VInt 1, VInt 0)), [VBool false, VBool true]);
