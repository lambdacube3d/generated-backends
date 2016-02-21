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

procedure :: String -> [Arg] -> Type -> StmtM () -> DefM ()
procedure = undefined

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

break_ :: StmtM ()
break_ = undefined

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

(.=) :: Exp -> Exp -> StmtM ()
(.=) = undefined

nullptr :: Exp
nullptr = undefined

inc :: Exp -> Exp
inc = undefined

value :: Exp -> Exp
value = undefined

key :: Exp -> Exp
key = undefined

expIf :: Exp -> Exp -> Exp -> Exp
expIf = undefined

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

for :: StmtM () -> Exp -> Exp -> StmtM () -> StmtM ()
for = undefined

(/) :: Exp -> Exp -> Exp
(/) = undefined

(==) :: Exp -> Exp -> Exp
(==) = undefined

(<=) :: Exp -> Exp -> Exp
(<=) = undefined

(/=) :: Exp -> Exp -> StmtM ()
(/=) = undefined

instance Num Exp where
  _ * _ = ()
  _ - _ = ()
  _ + _ = ()
  fromInteger _ = ()

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