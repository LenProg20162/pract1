{-
Facultad de Ciencias UNAM
   Lenguajes de programación 2016-2
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Ayudante lab: C. Moisés Vázquez Reyes
-}


module EABdinamica where

-- Se importa el módulo de entrada y salida así como los analizadores léxico y sintáctico.
-- Tambien se incorpora la semántica estática para evaluar expresiones bien tipadas.
import System.IO
import LexerEAB
import ParserEAB
import EABestatica


--   Sustitución
-- subst e x r  debe devolver e[x:=r].
sust :: Asa -> Ident -> Asa -> Asa
sust (VNum n) x r = VNum n
sust (VBol b) x r = VBol b
sust (Var x) y r = if (y == x) then r else (Var x)
sust (Suma n m) x r = (Suma (sust n x r) (sust m x r))
sust (Prod n m) x r = (Prod (sust n x r) (sust m x r))
sust (Let (Var z) e1 e2) x r = if (elem z ([x] ++ freevars r))
  then error ("Necesito alfa equivalencia")
  else (Let (Var z) (sust e1 x r) (sust e2 x r))
sust (Ifte e1 e2 e3) x r = (Ifte (sust e1 x r)(sust e2 x r)(sust e3 x r))
sust (Suc n) x r = (Suc (sust n x r))
sust (Pred n) x r = (Pred (sust n x r))
sust (Iszero n) x r = (Iszero (sust n x r))

freevars :: Asa -> [Ident]
freevars (VNum n) = []
freevars (VBol b) = []
freevars (Var x) = [x]
freevars (Suma n m) = freevars n ++ freevars m
freevars (Prod n m) = freevars (Suma n m)
freevars (Let (Var z) e1 e2) = filter (z/=) (freevars e2)
freevars (Ifte e1 e2 e3) = freevars e1 ++ freevars e2 ++ freevars e3
freevars (Suc n) = freevars n
freevars (Pred n) = freevars n
freevars (Iszero n) = freevars n


--   Valores 
-- Función que nos dice cuándo una expresión es un valor.
esvalor :: Asa -> Bool
esvalor = error "Te toca"


-- Evaluación de expresiones
-- Evalúa las expresiones que están bien tipadas.
eval :: Asa -> Asa
eval = error "Te toca"


-- evalaux hace transiciones mientras no se llegue a un estado final.
evalaux :: Asa -> Asa
evalaux = error "Te toca"


-- Reglas de transición
-- eval1p hace una transición mientras se pueda aplicar una regla de transición.
eval1p :: Asa -> Asa
eval1p (VBol a) = VBol a
eval1p (VNum a) = VNum a
eval1p (Var x) = errror "la variable "++x++"no esta ligada"
eval1p (Suma (VNum n) (VNum m)) = (Vnum (n+m))
eval1p (Suma t (VNum m)) = Suma (eval1p t) (VNum m)
eval1p (Suma t1 t2) = Suma t1 (eval1p t2) 
eval1p (Prod (VNum n) (VNum m)) = (Vnum (n*m))
eval1p (Prod t (VNum m)) = Prod (eval1p t)(VNum m)
eval1p (Prod t1 t2) = Prod (t1)(eval1p t2)
eval1p (Let (Var x) e1 e2) = (sust e2 (Var x) e1)
eval1p (Ifte (VBol True) t2 t3) = t2
eval1p (Ifte (VBol False) t2 t3) = t3
eval1p (Ifte  t1 t2 t3) = (Ifte  (eval1p t1) t2 t3)
eval1p (Suc (VNum n)) = (VNum (n+1)) 
eval1p (Suc t1) = (Suc (eval1p t1))
eval1p (Pred (VNum n)) = (Pred (n-1)) 
eval1p (Pred t1) = (Pred (eval1p t1))
eval1p (Iszero (VNum a)) = if (a == 0) then True else False
eval1p (Iszero t) = (Iszero (eval1p t)) 


-- 5 Pretty printer
-- Función que transforma un ASA a una expresión en sintaxis concreta. 
concreta :: Asa -> String
concreta (VNum n) = show n
concreta (VBol b) = show b
concreta (Var x) = x
concreta (Suma (VNum n) (VNum m)) = show n ++ " + " ++ show m 
concreta (Suma t (VNum m)) = "("++concreta t ++ ") + " ++ show m 
concreta (Suma (VNum n) t) = show n ++ " + (" ++ concreta t ++ ")"
concreta (Suma t1 t2) =  "("++concreta t1 ++ ") + (" ++ concreta t2 ++ ")"
concreta (Prod (VNum n) (VNum m)) = show n ++ " * " ++ show m 
concreta (Prod t (VNum m)) = "("++concreta t ++ ") * " ++ show m 
concreta (Prod (VNum n) t) = show n ++ " * (" ++ concreta t ++ ")"
concreta (Prod t1 t2) =  "("++concreta t1 ++ ") * (" ++ concreta t2 ++ ")"
concreta (Let (Var x) e1 e2) = "let " ++ x ++ ":=("++ concreta e1 ++ ") in ("++ concreta e2 ++ ")"
concreta (Ifte t1 t2 t3) = "if ("++ concreta t1 ++") then ("++ concreta t2 ++") else ("++ concreta t3 ++")" 
concreta (Suc t) =  "suc"++ "("++ concreta t ++")"
concreta (Pred t) = "pred"++ "("++ concreta t ++")"
concreta (Iszero t) = "isZero("++ concreta t ++ ")"


-- Pruebas
-- Función que recibe el nombre de un archivo que contiene una expresión en sintaxis concreta y la evalúa mostrando el proceso paso a paso.
correPrueba file = do cont <- readFile (file++".eab");       
                      putStr "\n\n";
                      putStr "<<< CONTENIDO DEL ARCHIVO >>>\n";
                      putStr cont;
                      putStr "\n\n";
                      putStr "<<< TOKENS GENERADOS POR EL LEXER >>>\n";
                      let lex = (lexer cont);
                      putStr (show lex);
                      putStr "\n\n";
                      putStr "<<< ASA  GENERADO POR EL PARSER >>>\n";
                      let par = (parse lex);
                      putStr (show par);
                      putStr "\n\n";
                      let tipo = (vt [] par);
                      putStr "<<< TIPO ASA >>>\n";
                      putStr (show tipo);
                      putStr "\n\n";
                      putStr "<<< ASA EVALUADO >>>\n";
                      let eva = (eval par);
                      putStr (show eva);
                      putStr "\n\n";
                      putStr "<<< RESULTADO >>>\n";
                      putStr (concreta (eval (parse (lexer cont))));
                      putStr "\n\n";                         
      
