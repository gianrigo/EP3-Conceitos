datatype Id = char
datatype valor = ValorInteiro of int | ValorBooleano of bool

datatype Expressao =
  Valor of valor
| Id of Id
| ExpBinaria of expBinaria
(*| IfThenElse of (string * ExpBinaria * Expressao * Expressao)*)
and
expBinaria = ExpBool of (Expressao * string * Expressao)
           | ExpAritm of (Expressao * string * Expressao)

exception Eval_Error of string

fun is_value(e:Expressao):bool =
  case e of
    Valor _ => true
  | Id _ => false
  | ExpBinaria(_) => false

fun subst(v:Expressao, x:Id, e:Expressao):Expressao =
  case e of
    Valor _ => e
  | ExpBinaria(ExpBool(e1,b,e3)) =>  ExpBinaria(ExpBool(subst(v,x,e1), b, subst(v,x,e3)))
  | ExpBinaria(ExpAritm(e1,a,e3)) => ExpBinaria(ExpAritm(subst(v,x,e1), a, subst(v,x,e3)))

fun eval_op(v1:valor, b:string, v2:valor):valor =
  case (v1, b, v2) of
    (ValorInteiro i1, "+", ValorInteiro i2) => ValorInteiro(i1+i2)
  | (ValorInteiro i1, "-", ValorInteiro i2) => ValorInteiro(i1-i2)
  | (ValorBooleano b1, "Or", ValorBooleano b2) => ValorBooleano(b1 orelse b2)
  | (ValorBooleano b1, "And", ValorBooleano b2) => ValorBooleano(b1 andalso b2)
  | (ValorBooleano b1, "==", ValorBooleano b2) => if (b1 = b2) then ValorBooleano(true) else ValorBooleano(false)
  | (_,_,_) => raise Eval_Error("Expressão binária inválida.")


fun eval'(e:Expressao):Expressao =
  case e of
    Valor v => Valor v
  | ExpBinaria(ExpBool (e1, b, e2)) =>
      let val v1 = eval e1
          val v2 = eval e2
      in
        case (v1, v2) of
          (Valor v1, Valor v2) => Valor(eval_op(v1, b, v2))
          | _ => raise Eval_Error("Expressão binária incorreta.")
      end
  | ExpBinaria(ExpAritm (e1, a, e2)) =>
      let val v1 = eval e1
          val v2 = eval e2
      in
        case (v1, v2) of
          (Valor v1, Valor v2) => Valor(eval_op(v1, a, v2))
          | _ => raise Eval_Error("Expressão aritmética incorreta.")
      end
  | Id _ => raise Eval_Error("Variável livre.") 
(*   | IfThenElse ("if",b,e1,e2) => if Valor(eval b)
                                   then eval e1
                                   else eval e2*)
and eval(e:Expressao):Expressao =
  if is_value e then e else
       eval'(e)
       
(* TESTES *)
val e1 = ExpBinaria(ExpAritm(Valor(ValorInteiro 300), "+", Valor(ValorInteiro 12)));
val e2 = ExpBinaria(ExpAritm(Valor(ValorInteiro 300), "-", Valor(ValorInteiro 12)));
val e3 = ExpBinaria(ExpBool(Valor(ValorBooleano true), "And", Valor(ValorBooleano false)));
val e4 = ExpBinaria(ExpBool(Valor(ValorBooleano true), "Or", Valor(ValorBooleano false)));
val e5 = ExpBinaria(ExpBool(Valor(ValorBooleano true), "==", Valor(ValorBooleano false)));





