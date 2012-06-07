datatype Id = char
datatype valor = ValorInteiro of int | ValorBooleano of bool
datatype binop = Mais | Menos | E | OU | IGUAL
datatype Expressao =
  Valor of valor
| Id of Id
| ExpBool of (Expressao * binop * Expressao)
| ExpAritm of (Expressao * binop * Expressao)

exception Eval_Error of string


fun is_value(e:Expressao):bool =
  case e of
    Valor _ => true
  | Id _ => false
  | ExpBool(_,_,_) => false
  | ExpAritm(_,_,_) => false

fun subst(v:Expressao, x:Id, e:Expressao):Expressao =
  case e of
    Valor _ => e
  | ExpBool(e1,b,e3) =>  ExpBool(subst(v,x,e1), b, subst(v,x,e3))
  | ExpAritm(e1,a,e3) =>  ExpAritm(subst(v,x,e1), a, subst(v,x,e3))

fun eval_binop(v1:valor, b:binop, v2:valor):valor =
  case (v1, b, v2) of
    (ValorInteiro i1, Mais, ValorInteiro i2) => ValorInteiro(i1+i2)
  | (ValorInteiro i1, Menos, ValorInteiro i2) => ValorInteiro(i1-i2)
  | (ValorBooleano b1, OU, ValorBooleano b2) => ValorBooleano(b1 orelse b2)
  | (ValorBooleano b1, E, ValorBooleano b2) => ValorBooleano(b1 andalso b2)
  | (ValorBooleano b1, IGUAL, ValorBooleano b2) => if (b1 = b2) then ValorBooleano(true) else ValorBooleano(false)
(*  | (_,_,_) => raise Eval_Error("type mismatch for binop") *)


fun eval'(e:Expressao):Expressao =
  case e of
    Valor v => Valor v
  | ExpBool (e1, b, e2) =>
      let val v1 = eval e1
          val v2 = eval e2
      in
        case (v1, v2) of
          (Valor v1, Valor v2) => Valor(eval_binop(v1, b, v2))
          | _ => raise Eval_Error("bad binop expression")
      end
  | ExpAritm (e1, a, e2) =>
      let val v1 = eval e1
          val v2 = eval e2
      in
        case (v1, v2) of
          (Valor v1, Valor v2) => Valor(eval_binop(v1, a, v2))
          | _ => raise Eval_Error("bad binop expression")
      end
    | Id _ => raise Eval_Error("unbound Idiable") 
and eval(e:Expressao):Expressao =
  if is_value e then e else
       eval'(e)
       
(* TESTES *)
val e1 = ExpAritm(Valor(ValorInteiro 300), Mais, Valor(ValorInteiro 12));
val e2 = ExpAritm(Valor(ValorInteiro 300), Menos, Valor(ValorInteiro 12));
val e3 = ExpBool(Valor(ValorBooleano true), E, Valor(ValorBooleano false));
val e4 = ExpBool(Valor(ValorBooleano true), OU, Valor(ValorBooleano false));
val e5 = ExpBool(Valor(ValorBooleano true), IGUAL, Valor(ValorBooleano false));





