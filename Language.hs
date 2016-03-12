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

data Exp
  = Exp :-> Exp
  | Exp :. Exp
  | Var String
  | Ns [String]
  | Integer Integer
  | FloatLit Float
  | BoolLit Bool
  | Cast Type Exp
  | Addr Exp
  | Deref Exp
  | Exp :+ Exp
  | Exp :- Exp
  | Exp :* Exp
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
  | New Type [Exp]
  | IteratorValue Exp -- used with foreach
  | IteratorKey Exp
  | RecordValue [(String,Exp)]
  | NotNull Exp
  | Not Exp
  | Map_notElem Exp Exp
  | Map_elem Exp Exp
  | Map_lookup Exp Exp
  | Vector_lookup Exp Exp
  | Vector_size Exp
  | Vector_dataPtr Exp
  | CallTypeConsructor Type Exp
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
  | For_range String Exp Exp [Stmt]
  | Exp :/= Exp
  | Exp :|= Exp
  | Exp :+= Exp
  | Map_foreach String Exp [Stmt]
  | Vector_foreach String Exp [Stmt]
  | Map_insert Exp Exp Exp
  | Break
  | Continue
  | Inc Exp
  | Vector_pushBack Exp Exp
  | Vector_pushBackPtr Exp Exp
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

data ClassDef
  = Method String [Arg] Type [Stmt]
  | Constructor [Arg] [Stmt]
  | Destructor [Stmt]
  | ClassVar Type [String]
  | ClassUnion [Arg]
  deriving Show

data ClassScope
  = Public  [ClassDef]
  | Private [ClassDef]
  deriving Show

data StructDef
  = StructVar Type [String]
  | StructUnion [Arg]
  deriving Show

data Def
  = Procedure String [Arg] Type [Stmt]
  | ClassDef  String [ClassScope]
  | EnumDef   String [String]
  | StructDef String [StructDef]
  deriving Show

type ClassScopeM = Writer [ClassScope]
type ClassDefM = Writer [ClassDef]
type StructDefM = Writer [StructDef]
type DefM = Writer [Def]

type IfM = Writer [If]
type StmtM = Writer [Stmt]
type CaseM = Writer [Case]

-- class
class_ :: String -> ClassScopeM () -> DefM ()
class_ className classM = tell [ClassDef className (execWriter classM)]

private :: ClassDefM () -> ClassScopeM ()
private classDefM = tell [Private (execWriter classDefM)]

public :: ClassDefM () -> ClassScopeM ()
public classDefM = tell [Public (execWriter classDefM)]

classVar :: Type -> [String] -> ClassDefM ()
classVar t n = tell [ClassVar t n]

classUnion :: [Arg] -> ClassDefM ()
classUnion args = tell [ClassUnion args]

constructor :: [Arg] -> StmtM () -> ClassDefM ()
constructor args stmtM = tell [Constructor args (execWriter stmtM)]

destructor :: StmtM () -> ClassDefM ()
destructor stmtM = tell [Destructor (execWriter stmtM)]

method :: String -> [Arg] -> Type -> StmtM () -> ClassDefM ()
method name args retType stmtM = tell [Method name args retType (execWriter stmtM)]

-- struct
struct_ :: String -> StructDefM () -> DefM ()
struct_ name structDefM = tell [StructDef name (execWriter structDefM)]

structVar :: Type -> [String] -> StructDefM ()
structVar t n = tell [StructVar t n]

structUnion :: [Arg] -> StructDefM ()
structUnion args = tell [StructUnion args]

-- enum
enum_ :: String -> [String] -> DefM ()
enum_ name args = tell [EnumDef name args]

-- function
procedure :: String -> [Arg] -> Type -> StmtM () -> DefM ()
procedure name args retType stmtM = tell [Procedure name args retType (execWriter stmtM)]

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

new :: Type -> [Exp] -> Exp
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

callTypeConsructor :: Type -> Exp -> Exp
callTypeConsructor = CallTypeConsructor

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
for_range a b c stmtM = tell [For_range a b c (execWriter stmtM)]

map_foreach :: String -> Exp -> StmtM () -> StmtM ()
map_foreach n e stmtM = tell [Map_foreach n e (execWriter stmtM)]

vector_foreach :: String -> Exp -> StmtM () -> StmtM ()
vector_foreach n e stmtM = tell [Vector_foreach n e (execWriter stmtM)]

vector_pushBack :: Exp -> Exp -> StmtM ()
vector_pushBack a b = tell [Vector_pushBack a b]

vector_pushBackPtr :: Exp -> Exp -> StmtM ()
vector_pushBackPtr a b = tell [Vector_pushBackPtr a b]

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
a += b = tell [a :+= b]

(!=) :: Exp -> Exp -> Exp
(!=) = (:!=)

instance Num Exp where
  (*) = (:*)
  (-) = (:-)
  (+) = (:+)
  fromInteger i = Integer i

instance Fractional Exp where
  fromRational i = FloatLit $ fromRational i

instance IsString Exp where
  fromString n = Var n

instance IsString Type where
  fromString n = Class n
