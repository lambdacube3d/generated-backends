{-# LANGUAGE TypeSynonymInstances #-}
module Language where

import Data.String
import Control.Monad.Writer

data Type
  = Bool
  | Float
  | Int
  | UInt
  | Void
  | Class String
  | Enum String
  | Const Type
  | Ref Type      -- &
  | Ptr Type      -- *
  | SmartPtr Type -- shared_ptr
  deriving Show

type IfM = Writer [If]
type DefM = Writer [Def]
type StmtM = Writer [Stmt]
type CaseM = Writer [Case]

data Exp
  = Exp :-> Exp
  | Var String
  | Ns [String]
  deriving Show

data Def
  = Procedure String [Arg] Type [Stmt]
  deriving Show

data Arg = String :. Type deriving Show

data Stmt
  = Switch Exp [Case]
  | Return Exp
  | Throw  String
  deriving Show

data Case
  = Case Pat [Stmt]
  | Default [Stmt]
  deriving Show

data If = If

data Pat
  = NsPat [String]
  deriving Show


method :: String -> String -> [Arg] -> Type -> StmtM () -> DefM ()
method = error "method"

procedure :: String -> [Arg] -> Type -> StmtM () -> DefM ()
procedure name args retType stmtM = tell [Procedure name args retType (execWriter stmtM)]

constructor :: String -> [Arg] -> StmtM () -> DefM ()
constructor = error "constructor"

destructor :: String -> StmtM () -> DefM ()
destructor = error "destructor"

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
continue_ = error "continue_"

break_ :: StmtM ()
break_ = error "break_"

map_notElem :: Exp -> Exp -> Exp
map_notElem = error "map_notElem"

map_elem :: Exp -> Exp -> Exp
map_elem = error "map_elem"

deref :: Exp -> Exp
deref = error "deref"

not :: Exp -> Exp
not = error "not"

notNull :: Exp -> Exp
notNull = error "notNull"

(~>) :: Exp -> Exp -> Exp
(~>) = (:->)

(.) :: Exp -> Exp -> Exp
(.) = error "."

call :: Exp -> [Exp] -> StmtM ()
call = error "call"

callExp :: Exp -> [Exp] -> Exp
callExp = error "callExp"

new :: String -> [Exp] -> Exp
new = error "new"

cast :: Type -> Exp -> Exp
cast = error "cast"

addr :: Exp -> Exp
addr = error "addr"

false :: Exp
false = error "false"

true :: Exp
true = error "true"

if_ :: Exp -> IfM () -> StmtM ()
if_ = error "if_"

then_ :: StmtM () -> IfM ()
then_ = error "then_"

else_ :: StmtM () -> IfM ()
else_ = error "else_"

vector_lookup :: Exp -> Exp -> Exp
vector_lookup = error "vector_lookup"

map_lookup :: Exp -> Exp -> Exp
map_lookup = error "map_lookup"

infix 1 .=
(.=) :: Exp -> Exp -> StmtM ()
(.=) = error ".="

nullptr :: Exp
nullptr = error "nullptr"

incExp :: Exp -> Exp
incExp = error "incExp"

inc :: Exp -> StmtM ()
inc = error "inc"

value :: Exp -> Exp
value = error "value"

key :: Exp -> Exp
key = error "key"

expIf :: Exp -> Exp -> Exp -> Exp
expIf = error "expIf"

recordValue :: [(String,Exp)] -> Exp
recordValue = error "recordValue"

varCharPtrFromString :: String -> Exp -> StmtM ()
varCharPtrFromString = error "varCharPtrFromString"

charPtrFromString :: Exp -> Exp
charPtrFromString = error "charPtrFromString"

var :: Type -> [String] -> StmtM ()
var = error "var"

varADT :: String -> String -> Exp -> StmtM ()
varADT = error "varADT"

varAssign :: Type -> String -> Exp -> StmtM ()
varAssign = error "varAssign"

varConstructor :: Type -> String -> Exp -> StmtM ()
varConstructor = error "varConstructor"

map_foreach :: String -> Exp -> StmtM () -> StmtM ()
map_foreach = error "map_foreach"

vector_foreach :: String -> Exp -> StmtM () -> StmtM ()
vector_foreach = error "vector_foreach"

vector_pushBack :: Exp -> Exp -> StmtM ()
vector_pushBack = error "vector_pushBack"

for :: StmtM () -> Exp -> Exp -> StmtM () -> StmtM ()
for = error "for"

(/) :: Exp -> Exp -> Exp
(/) = error "/"

(&&) :: Exp -> Exp -> Exp
(&&) = error "&&"

(==) :: Exp -> Exp -> Exp
(==) = error "=="

(<=) :: Exp -> Exp -> Exp
(<=) = error "<="

(>=) :: Exp -> Exp -> Exp
(>=) = error ">="

(|=) :: Exp -> Exp -> StmtM ()
(|=) = error "|="

(/=) :: Exp -> Exp -> StmtM ()
(/=) = error "/="

(!=) :: Exp -> Exp -> Exp
(!=) = error "!="

instance Num Exp where
  _ * _ = error "*"
  _ - _ = error "-"
  _ + _ = error "+"
  fromInteger _ = error "fromInteger"

instance Fractional Exp where
  fromRational _ = error "fromRational"

instance IsString Exp where
  fromString n = Var n

instance IsString Type where
  fromString n = Class n
