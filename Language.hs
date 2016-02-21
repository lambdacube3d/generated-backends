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

type DefM = Writer [Def]
type StmtM = Writer [Stmt]
type CaseM = Writer [Case]

type Exp = ()

type Def = ()

data Arg = String :. Type

type Stmt = ()
type Case = ()

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

(~>) :: Exp -> Exp -> Exp
(~>) = undefined

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