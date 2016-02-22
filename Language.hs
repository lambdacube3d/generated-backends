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

type IfM = Writer [If]
type DefM = Writer [Def]
type StmtM = Writer [Stmt]
type CaseM = Writer [Case]

type Exp = ()

type Def = ()

data Arg = String :. Type

type Stmt = ()
type Case = ()
type If = ()

method :: String -> String -> [Arg] -> Type -> StmtM () -> DefM ()
method = undefined

procedure :: String -> [Arg] -> Type -> StmtM () -> DefM ()
procedure = undefined

constructor :: String -> [Arg] -> StmtM () -> DefM ()
constructor = undefined

destructor :: String -> StmtM () -> DefM ()
destructor = undefined

switch :: Exp -> CaseM () -> StmtM ()
switch = undefined

case_ :: Pat -> StmtM () -> CaseM ()
case_ = undefined

default_ :: StmtM () -> CaseM ()
default_ = undefined

throw :: String -> StmtM ()
throw = undefined

type Pat = ()
ns :: [String] -> Pat
ns = undefined

return_ :: Exp -> StmtM ()
return_ = undefined

continue_ :: StmtM ()
continue_ = undefined

break_ :: StmtM ()
break_ = undefined

map_notElem :: Exp -> Exp -> Exp
map_notElem = undefined

map_elem :: Exp -> Exp -> Exp
map_elem = undefined

deref :: Exp -> Exp
deref = undefined

not :: Exp -> Exp
not = undefined

notNull :: Exp -> Exp
notNull = undefined

(~>) :: Exp -> Exp -> Exp
(~>) = undefined

(.) :: Exp -> Exp -> Exp
(.) = undefined

call :: Exp -> [Exp] -> StmtM ()
call = undefined

callExp :: Exp -> [Exp] -> Exp
callExp = undefined

new :: String -> [Exp] -> Exp
new = undefined

cast :: Type -> Exp -> Exp
cast = undefined

addr :: Exp -> Exp
addr = undefined

false :: Exp
false = undefined

true :: Exp
true = undefined

if_ :: Exp -> IfM () -> StmtM ()
if_ = undefined

then_ :: StmtM () -> IfM ()
then_ = undefined

else_ :: StmtM () -> IfM ()
else_ = undefined

vectorLookup :: Exp -> Exp -> Exp
vectorLookup = undefined

mapLookup :: Exp -> Exp -> Exp
mapLookup = undefined

infix 1 .=
(.=) :: Exp -> Exp -> StmtM ()
(.=) = undefined

nullptr :: Exp
nullptr = undefined

incExp :: Exp -> Exp
incExp = undefined

inc :: Exp -> StmtM ()
inc = undefined

value :: Exp -> Exp
value = undefined

key :: Exp -> Exp
key = undefined

expIf :: Exp -> Exp -> Exp -> Exp
expIf = undefined

recordValue :: [(String,Exp)] -> Exp
recordValue = undefined

varCharPtrFromString :: String -> Exp -> StmtM ()
varCharPtrFromString = undefined

charPtrFromString :: Exp -> Exp
charPtrFromString = undefined

var :: Type -> [String] -> StmtM ()
var = undefined

varADT :: String -> String -> Exp -> StmtM ()
varADT = undefined

varAssign :: Type -> String -> Exp -> StmtM ()
varAssign = undefined

varConstructor :: Type -> String -> Exp -> StmtM ()
varConstructor = undefined

map_foreach :: String -> Exp -> StmtM () -> StmtM ()
map_foreach = undefined

vector_foreach :: String -> Exp -> StmtM () -> StmtM ()
vector_foreach = undefined

vector_pushBack :: Exp -> Exp -> StmtM ()
vector_pushBack = undefined

for :: StmtM () -> Exp -> Exp -> StmtM () -> StmtM ()
for = undefined

(/) :: Exp -> Exp -> Exp
(/) = undefined

(&&) :: Exp -> Exp -> Exp
(&&) = undefined

(==) :: Exp -> Exp -> Exp
(==) = undefined

(<=) :: Exp -> Exp -> Exp
(<=) = undefined

(>=) :: Exp -> Exp -> Exp
(>=) = undefined

(|=) :: Exp -> Exp -> StmtM ()
(|=) = undefined

(/=) :: Exp -> Exp -> StmtM ()
(/=) = undefined

(!=) :: Exp -> Exp -> Exp
(!=) = undefined

instance Num Exp where
  _ * _ = ()
  _ - _ = ()
  _ + _ = ()
  fromInteger _ = ()

instance Fractional Exp where
  fromRational _ = ()

instance IsString Exp where
  fromString _ = ()

instance IsString Type where
  fromString n = Class n


{-
data Statement
return_
switch
  case_
  default
if_
  then_
  else_
vector_foreach
map_foreach
break_
continue_
method
procedure
throw
varADT
var
varAssign
varConstructor
new
vector_pushBack

data Expression
(&&)
(+)
(->)
(.)
(/)
(/=)
(<=)
(==)
(>=)
true
false
expIf
`mapLookup`
`vectorLookup`
addr
call
cast
charPtrFromString
deref
inc
map_elem
map_notElem
not
notNull
nullptr
recordValue
key
value

-- language
(.:)
(.=)
ns
-}