{-# LANGUAGE TypeSynonymInstances #-}
module Language where

import Data.String
import Control.Monad.Writer

data Type
  = Bool
  | Float
  | Int
  | Int8
  | Int16
  | Long
  | UInt
  | UInt8
  | UInt16
  | Void
  | Class String
  | Enum String
  | Const Type
  | Ref Type      -- &
  | Ptr Type      -- *
  | SmartPtr Type -- shared_ptr
  | Vector Type
  | String
  | Map Type Type
  deriving Show

type IfM = Writer [If]
type DefM = Writer [Def]
type ClassM = Writer [Class]
type StmtM = Writer [Stmt]
type CaseM = Writer [Case]

data Exp
  = Exp :-> Exp
  | Exp :. Exp
  | Var String
  | Ns [String]
  | Integer Integer
  | FloatLit Float
  | Cast Type Exp
  | Addr Exp
  | Deref Exp
  | BoolLit Bool
  | Vector_lookup Exp Exp
  | Map_lookup Exp Exp
  | Exp :+ Exp
  | Exp :/ Exp
  | Exp :!= Exp
  | Exp :== Exp
  | Exp :<= Exp
  | Exp :>= Exp
  | Exp :&& Exp
  | IncExp Exp
  | CallExp Exp [Exp]
  | ExpIf Exp Exp Exp
  | NullPtr
  | CharPtrFromString Exp
  | New String [Exp]
  | IteratorValue Exp -- used with foreach
  | IteratorKey Exp
  | RecordValue [(String,Exp)]
  | NotNull Exp
  | Not Exp
  | Map_notElem Exp Exp
  | Map_elem Exp Exp
  | Vector_size Exp
  | Vector_dataPtr Exp
  | New_SmartPtr Exp
  deriving Show

data Def
  = Procedure String [Arg] Type [Stmt]
  | Method String String [Arg] Type [Stmt]
  | Constructor String [Arg] [Stmt]
  | Destructor String [Stmt]
  deriving Show

data Arg = String :@ Type deriving Show

data Stmt
  = Switch Exp [Case]
  | Return Exp
  | Throw  String
  | Call Exp [Exp]
  | If Exp [Stmt] [Stmt]
  | VarDef Type [String]
  | VarADTDef String String Exp
  | VarAssignDef Type String Exp
  | VarConstructor Type String Exp
  | VarCharPtrFromString String Exp
  | Exp := Exp
  | For [Stmt] Exp Exp [Stmt]
  | Exp :/= Exp
  | Exp :|= Exp
  | Map_foreach String Exp [Stmt]
  | Vector_foreach String Exp [Stmt]
  | Vector_pushBack Exp Exp
  | Map_insert Exp Exp Exp
  | Break
  | Continue
  | Inc Exp
  deriving Show

data Case
  = Case Pat [Stmt]
  | Default [Stmt]
  deriving Show

data If
  = Then [Stmt]
  | Else [Stmt]
  deriving Show

data Pat
  = NsPat [String]
  deriving Show

data Class = Class_

class_ :: String -> ClassM () -> DefM ()
class_ className classM = return () -- TODO

private :: DefM () -> ClassM ()
private _ = return () -- TODO

public :: DefM () -> ClassM ()
public _ = return () -- TODO

memberVar :: Type -> [String] -> DefM ()
memberVar t n = return () -- TODO

enum_ :: String -> [String] -> DefM ()
enum_ name args = return () -- TODO

struct_ :: String -> DefM () -> DefM ()
struct_ name args = return () -- TODO

memberUnion :: [Arg] -> DefM ()
memberUnion args = return () -- TODO

method :: String -> [Arg] -> Type -> StmtM () -> DefM ()
method name args retType stmtM = tell [Method "" name args retType (execWriter stmtM)]

procedure :: String -> [Arg] -> Type -> StmtM () -> DefM ()
procedure name args retType stmtM = tell [Procedure name args retType (execWriter stmtM)]

constructor :: [Arg] -> StmtM () -> DefM ()
constructor args stmtM = tell [Constructor "" args (execWriter stmtM)]

destructor :: StmtM () -> DefM ()
destructor stmtM = tell [Destructor "" (execWriter stmtM)]

switch :: Exp -> CaseM () -> StmtM ()
switch exp caseM = tell [Switch exp (execWriter caseM)]

case_ :: Pat -> StmtM () -> CaseM ()
case_ pat stmtM = tell [Case pat (execWriter stmtM)]

default_ :: StmtM () -> CaseM ()
default_ stmtM = tell [Default (execWriter stmtM)]

throw :: String -> StmtM ()
throw msg = tell [Throw msg]

ns :: [String] -> Exp
ns = Ns

nsPat :: [String] -> Pat
nsPat = NsPat

return_ :: Exp -> StmtM ()
return_ exp = tell [Return exp]

continue_ :: StmtM ()
continue_ = tell [Continue]

break_ :: StmtM ()
break_ = tell [Break]

map_notElem :: Exp -> Exp -> Exp
map_notElem = Map_notElem

map_elem :: Exp -> Exp -> Exp
map_elem = Map_elem

deref :: Exp -> Exp
deref = Deref

not :: Exp -> Exp
not = Not

notNull :: Exp -> Exp
notNull = NotNull

(~>) :: Exp -> Exp -> Exp
(~>) = (:->)

(.) :: Exp -> Exp -> Exp
(.) = (:.)

call :: Exp -> [Exp] -> StmtM ()
call fun args = tell [Call fun args]

callExp :: Exp -> [Exp] -> Exp
callExp = CallExp

new :: String -> [Exp] -> Exp
new = New

cast :: Type -> Exp -> Exp
cast = Cast

addr :: Exp -> Exp
addr = Addr

false :: Exp
false = BoolLit False

true :: Exp
true = BoolLit True

if_ :: Exp -> IfM () -> StmtM ()
if_ exp ifM = tell [If exp (concat [a | Then a <- l]) (concat [a | Else a <- l])]
  where l = execWriter ifM

then_ :: StmtM () -> IfM ()
then_ stmtM = tell [Then (execWriter stmtM)]

else_ :: StmtM () -> IfM ()
else_ stmtM = tell [Else (execWriter stmtM)]

new_SmartPtr :: Exp -> Exp
new_SmartPtr = New_SmartPtr

vector_size :: Exp -> Exp
vector_size = Vector_size

vector_dataPtr :: Exp -> Exp
vector_dataPtr = Vector_dataPtr

map_insert :: Exp -> Exp -> Exp -> StmtM ()
map_insert m k v = tell [Map_insert m k v]

infixr 2 `vector_lookup`
infixr 2 `map_lookup`

vector_lookup :: Exp -> Exp -> Exp
vector_lookup = Vector_lookup

map_lookup :: Exp -> Exp -> Exp
map_lookup = Map_lookup

infix 1 .=
(.=) :: Exp -> Exp -> StmtM ()
a .= b = tell [a := b]

nullptr :: Exp
nullptr = NullPtr

incExp :: Exp -> Exp
incExp = IncExp

inc :: Exp -> StmtM ()
inc e = tell [Inc e]

it_value :: Exp -> Exp
it_value = IteratorValue

key :: Exp -> Exp
key = IteratorKey

expIf :: Exp -> Exp -> Exp -> Exp
expIf = ExpIf

recordValue :: [(String,Exp)] -> Exp
recordValue = RecordValue

varCharPtrFromString :: String -> Exp -> StmtM ()
varCharPtrFromString n e = tell [VarCharPtrFromString n e]

charPtrFromString :: Exp -> Exp
charPtrFromString = CharPtrFromString

var :: Type -> [String] -> StmtM ()
var t n = tell [VarDef t n]

varADT :: String -> String -> Exp -> StmtM ()
varADT t n e = tell [VarADTDef t n e ]

varAssign :: Type -> String -> Exp -> StmtM ()
varAssign t n e = tell [VarAssignDef t n e]

varConstructor :: Type -> String -> Exp -> StmtM ()
varConstructor t n e = tell [VarConstructor t n e]

for_range :: String -> Exp -> Exp -> StmtM () -> StmtM ()
for_range _ _ _ _ = return () -- TODO

map_foreach :: String -> Exp -> StmtM () -> StmtM ()
map_foreach n e stmtM = tell [Map_foreach n e (execWriter stmtM)]

vector_foreach :: String -> Exp -> StmtM () -> StmtM ()
vector_foreach n e stmtM = tell [Vector_foreach n e (execWriter stmtM)]

vector_pushBack :: Exp -> Exp -> StmtM ()
vector_pushBack a b = tell [Vector_pushBack a b]

for :: StmtM () -> Exp -> Exp -> StmtM () -> StmtM ()
for loopInitM cond exp stmtM = tell [For (execWriter loopInitM) cond exp (execWriter stmtM)]

(/) :: Exp -> Exp -> Exp
(/) = (:/)

(&&) :: Exp -> Exp -> Exp
(&&) = (:&&)

(==) :: Exp -> Exp -> Exp
(==) = (:==)

(<=) :: Exp -> Exp -> Exp
(<=) = (:<=)

(>=) :: Exp -> Exp -> Exp
(>=) = (:>=)

(|=) :: Exp -> Exp -> StmtM ()
a |= b = tell [ a :|= b]

(/=) :: Exp -> Exp -> StmtM ()
a /= b = tell [a :/= b]

(+=) :: Exp -> Exp -> StmtM ()
a += b = return () -- TODO

(!=) :: Exp -> Exp -> Exp
(!=) = (:!=)

instance Num Exp where
  _ * _ = error "*"
  _ - _ = error "-"
  (+) = (:+)
  fromInteger i = Integer i

instance Fractional Exp where
  fromRational i = FloatLit $ fromRational i

instance IsString Exp where
  fromString n = Var n

instance IsString Type where
  fromString n = Class n
