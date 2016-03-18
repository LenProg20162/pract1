{-
Facultad de Ciencias UNAM
   Lenguajes de programación 2016-2
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Ayudante lab: C. Moisés Vázquez Reyes
-}

module EABestatica where

-- Se importa el modulo de entrada y salida asi como los analizadores lexico y sintactico
import System.IO
import LexerEAB
import ParserEAB

-- Un tipo de datos para los tipos de EAB
data Tipo = TBol | TNat deriving (Show,Eq) 

-- Contextos como sinonimo de una lista de identificadores
type Ctx = [(Ident,Tipo)]


--Implementacion de la semantica estatica (Juicios para tipos)
vt :: Ctx -> Asa -> Tipo 
vt (xs) (VNum n) = TNat
vt (xs) (VBol True) = TBol
vt (xs) (VBol False) = TBol
vt [] (Var z) = error ("La variable " ++ show z ++ " no está declarada en el contexto")
vt (x:xs) (Var z) = if ((null (x:xs)) || ((filter ((==z).fst) (x:xs)) == []))
  then error ("La variable " ++ show z ++ " no está declarada en el contexto")
  else if (z == (fst x))
       then snd(x)
       else vt xs (Var z)
vt (xs) (Suma a1 a2) = if ( ((vt xs a1) == TNat) && ((vt xs a2) == TNat) )
  then TNat
  else error ("Los argumentos deben ser tipo TNat")
vt (xs) (Prod a1 a2) = vt xs (Suma a1 a2)
vt (xs) (Let (Var z) e1 e2) = vt (xs ++ [(z,t1)]) e2 where t1 = (vt (xs) e1)
vt (xs) (Ifte e1 e2 e3) = if ((vt xs e1) == TBol)
  then if ((vt xs e2) == (vt xs e3))
       then vt xs e2
       else error ("Los tipos de las ramas deben ser iguales")
  else error ("El tipo de la guardia debe ser booleano")
vt (xs) (Suc a) = if ((vt xs a) == TNat) then TNat else error ("El argumento debe tener tipo TNat")
vt (xs) (Pred a) = vt xs (Suc a)
vt (xs) (Iszero a) = if ((vt xs a) == TNat) then TBol else error ("El argumento debe tener tipo TBol")