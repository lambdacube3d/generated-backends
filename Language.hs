{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Language where

import Data.String
import Control.Monad.Writer

data GLPrim
  = GLGenTexture Exp
  | GLGenFramebuffer Exp
  | GLGenBuffer Exp
  | GLDeleteTexture Exp
  | GLDeleteFramebuffer Exp
  | GLShaderSource Exp Exp
  | GLGetUniformLocation Exp Exp Exp
  | GLGetAttribLocation Exp Exp Exp
  | GLUniform1fv Exp Exp Exp
  | GLUniform1iv Exp Exp Exp
  | GLUniform2fv Exp Exp Exp
  | GLUniform2iv Exp Exp Exp
  | GLUniform3fv Exp Exp Exp
  | GLUniform3iv Exp Exp Exp
  | GLUniform4fv Exp Exp Exp
  | GLUniform4iv Exp Exp Exp
  | GLUniformMatrix2fv Exp Exp Exp Exp
  | GLUniformMatrix3fv Exp Exp Exp Exp
  | GLUniformMatrix4fv Exp Exp Exp Exp
  | GLVertexAttrib1fv Exp Exp Exp
  | GLVertexAttrib2fv Exp Exp Exp
  | GLVertexAttrib3fv Exp Exp Exp
  | GLVertexAttrib4fv Exp Exp Exp
  | GLVertexAttribPointer Exp Exp Exp Exp Exp Exp
  deriving Show

data GLCommand -- GL ES 2.0
  = GLActiveTexture
  | GLAttachShader
  | GLBindBuffer
  | GLBindFramebuffer
  | GLBindTexture
  | GLBlendColor
  | GLBlendEquationSeparate
  | GLBlendFuncSeparate
  | GLBufferData
  | GLBufferSubData
  | GLClear
  | GLClearColor
  | GLClearDepthf
  | GLClearStencil
  | GLColorMask
  | GLCompileShader
  | GLCreateProgram
  | GLCreateShader
  | GLCullFace
  | GLDeleteFramebuffers
  | GLDeleteProgram
  | GLDeleteShader
  | GLDeleteTextures
  | GLDepthFunc
  | GLDepthMask
  | GLDisable
  | GLDisableVertexAttribArray
  | GLDrawArrays
  | GLEnable
  | GLEnableVertexAttribArray
  | GLFramebufferTexture2D
  | GLFrontFace
  | GLLineWidth
  | GLLinkProgram
  | GLPolygonOffset
  | GLReleaseShaderCompiler
  | GLTexImage2D
  | GLTexParameteri
  | GLUniform1i
  | GLUseProgram
  | GLViewport
  deriving Show

data GLConstant -- GL ES 2.0
  = GL_ALWAYS
  | GL_ARRAY_BUFFER
  | GL_BACK
  | GL_BLEND
  | GL_BYTE
  | GL_CCW
  | GL_CLAMP_TO_EDGE
  | GL_COLOR_ATTACHMENT0
  | GL_COLOR_BUFFER_BIT
  | GL_CONSTANT_ALPHA
  | GL_CONSTANT_COLOR
  | GL_CULL_FACE
  | GL_CW
  | GL_DEPTH_ATTACHMENT
  | GL_DEPTH_BUFFER_BIT
  | GL_DEPTH_COMPONENT
  | GL_DEPTH_TEST
  | GL_DST_ALPHA
  | GL_DST_COLOR
  | GL_EQUAL
  | GL_FLOAT
  | GL_FRAGMENT_SHADER
  | GL_FRAMEBUFFER
  | GL_FRONT
  | GL_FUNC_ADD
  | GL_FUNC_REVERSE_SUBTRACT
  | GL_FUNC_SUBTRACT
  | GL_GEQUAL
  | GL_GREATER
  | GL_LEQUAL
  | GL_LESS
  | GL_LINEAR
  | GL_LINEAR_MIPMAP_LINEAR
  | GL_LINEAR_MIPMAP_NEAREST
  | GL_LINES
  | GL_LINE_LOOP
  | GL_LINE_STRIP
  | GL_MIRRORED_REPEAT
  | GL_NEAREST
  | GL_NEAREST_MIPMAP_LINEAR
  | GL_NEAREST_MIPMAP_NEAREST
  | GL_NEVER
  | GL_NOTEQUAL
  | GL_ONE
  | GL_ONE_MINUS_CONSTANT_ALPHA
  | GL_ONE_MINUS_CONSTANT_COLOR
  | GL_ONE_MINUS_DST_ALPHA
  | GL_ONE_MINUS_DST_COLOR
  | GL_ONE_MINUS_SRC_ALPHA
  | GL_ONE_MINUS_SRC_COLOR
  | GL_POINTS
  | GL_POLYGON_OFFSET_FILL
  | GL_REPEAT
  | GL_RGBA
  | GL_SHORT
  | GL_SRC_ALPHA
  | GL_SRC_ALPHA_SATURATE
  | GL_SRC_COLOR
  | GL_STATIC_DRAW
  | GL_STENCIL_ATTACHMENT
  | GL_STENCIL_BUFFER_BIT
  | GL_STENCIL_TEST
  | GL_TEXTURE0
  | GL_TEXTURE_2D
  | GL_TEXTURE_CUBE_MAP
  | GL_TEXTURE_CUBE_MAP_NEGATIVE_X
  | GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
  | GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
  | GL_TEXTURE_CUBE_MAP_POSITIVE_X
  | GL_TEXTURE_CUBE_MAP_POSITIVE_Y
  | GL_TEXTURE_CUBE_MAP_POSITIVE_Z
  | GL_TEXTURE_MAG_FILTER
  | GL_TEXTURE_MIN_FILTER
  | GL_TEXTURE_WRAP_S
  | GL_TEXTURE_WRAP_T
  | GL_TRIANGLES
  | GL_TRIANGLE_FAN
  | GL_TRIANGLE_STRIP
  | GL_UNSIGNED_BYTE
  | GL_UNSIGNED_SHORT
  | GL_VERTEX_SHADER
  | GL_ZERO
  deriving Show

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
  | String
  | Void
  | Class String
  | Builtin String
  | Enum String
  | ADTEnum String
  | ADTCons String String
  | Vector Type
  | Map Type Type
  | NativeArray Type -- only for passing buffers
  | NativeBuffer
  deriving (Show,Eq)

data Exp
  = Var String
  | EnumVal String String
  | EnumADT String String
  | Integer Integer
  | FloatLit Float
  | BoolLit Bool
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
  | CallProcExp Exp [Exp]
  | ExpIf Exp Exp Exp
  | NullPtr
  | IteratorValue Exp -- used with foreach
  | IteratorKey Exp
  | NotNull Exp
  | Not Exp
  | Map_notElem Exp Exp
  | Map_elem Exp Exp
  | Map_lookup Exp Exp
  | Vector_lookup Exp Exp
  | Vector_size Exp
  | Exp :. Exp
  -- for C++
  | Exp :-> Exp
  -- GL stuff
  | GLCommand   GLCommand
  | GLConstant  GLConstant
  deriving Show

data Stmt
  = Switch Exp [Case]
  | Return Exp
  | Throw  String
  | Call Exp [Exp]
  | CallProc Exp [Exp]
  | CallGLPrim GLPrim
  | If Exp [Stmt] [Stmt]
  | VarDef Type [String]
  | VarADTDef String String String Exp
  | VarAssignDef Type String Exp
  | VarConstructor String String [Exp]
  | VarRecordValue Type String [(String,Exp)]
  | VarNativeBufferFrom Type String Exp
  | Exp := Exp
  | For [Stmt] Exp Exp [Stmt]
  | For_range String Exp Exp [Stmt]
  | Exp :/= Exp
  | Exp :|= Exp
  | Exp :+= Exp
  | Map_foreach Type Type String Exp [Stmt]
  | Vector_foreach Type String Exp [Stmt]
  | Vector_new Type String
  | Map_insert Exp Exp Exp
  | Break
  | Continue
  | Inc Exp
  | Vector_pushBack Exp Exp
  | AllocClassVars
  | AllocNativeArray Type Exp
  | CopyToNativeArray Type Exp Exp
  deriving Show

data Arg = String :@ Type deriving Show

data Case
  = Case Pat [Stmt]
  | Default [Stmt]
  deriving Show

data If
  = Then [Stmt]
  | Else [Stmt]
  deriving Show

data Pat
  = NsPat String String
  | NsPatADT String String
  | GLPat GLConstant
  deriving Show

data ClassDef
  = Method Bool{- isStatic -} String [Arg] Type [Stmt]
  | Constructor [Arg] [Stmt]
  | Destructor [Stmt]
  | ClassVar Type [String]
  deriving Show

data ClassScope
  = Public  [ClassDef]
  | Private [ClassDef]
  deriving Show

data Def
  = Procedure String [Arg] Type [Stmt]
  | ClassDef  String [ClassScope]
  | EnumDef   String [String]
  deriving Show

type ClassScopeM = Writer [ClassScope]
type ClassDefM = Writer [ClassDef]
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

constructor :: [Arg] -> StmtM () -> ClassDefM ()
constructor args stmtM = tell [Constructor args (execWriter stmtM)]

destructor :: StmtM () -> ClassDefM ()
destructor stmtM = tell [Destructor (execWriter stmtM)]

method :: String -> [Arg] -> Type -> StmtM () -> ClassDefM ()
method name args retType stmtM = tell [Method False name args retType (execWriter stmtM)]

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

nsPat :: String -> String -> Pat
nsPat = NsPat

glPat :: GLConstant -> Pat
glPat = GLPat

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

not :: Exp -> Exp
not = Not

notNull :: Exp -> Exp
notNull = NotNull

(~>) :: Exp -> Exp -> Exp
(~>) = (:->)

(.) :: Exp -> Exp -> Exp
(.) = (:.)

callGLPrim :: GLPrim -> StmtM ()
callGLPrim prim = tell [CallGLPrim prim]

callGL :: GLCommand -> [Exp] -> StmtM ()
callGL fun args = tell [Call (GLCommand fun) args]

callExpGL :: GLCommand -> [Exp] -> Exp
callExpGL a b = CallExp (GLCommand a) b

callProc :: Exp -> [Exp] -> StmtM ()
callProc fun args = tell [CallProc fun args]

callProcExp :: Exp -> [Exp] -> Exp
callProcExp a b = CallProcExp a b

call :: Exp -> [Exp] -> StmtM ()
call fun args = tell [Call fun args]

callExp :: Exp -> [Exp] -> Exp
callExp a b = CallExp a b

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

varNativeBufferFrom :: Type -> String -> Exp -> StmtM ()
varNativeBufferFrom t n v = tell [VarNativeBufferFrom t n v]

vector_size :: Exp -> Exp
vector_size = Vector_size

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
expIf a b c = ExpIf a b c

varRecordValue :: Type -> String -> [(String,Exp)] -> StmtM ()
varRecordValue t n l = tell [VarRecordValue t n l]

var :: Type -> [String] -> StmtM ()
var t n = tell [VarDef t n]

varADT :: String -> String -> String -> Exp -> StmtM ()
varADT t c n e = tell [VarADTDef t c n e ]

varAssign :: Type -> String -> Exp -> StmtM ()
varAssign t n e = tell [VarAssignDef t n e]

varConstructor :: String -> String -> [Exp] -> StmtM ()
varConstructor t n e = tell [VarConstructor t n e]

for_range :: String -> Exp -> Exp -> StmtM () -> StmtM ()
for_range a b c stmtM = tell [For_range a b c (execWriter stmtM)]

map_foreach :: Type -> Type -> String -> Exp -> StmtM () -> StmtM ()
map_foreach tk tv n e stmtM = tell [Map_foreach tk tv n e (execWriter stmtM)]

vector_foreach :: Type -> String -> Exp -> StmtM () -> StmtM ()
vector_foreach t n e stmtM = tell [Vector_foreach t n e (execWriter stmtM)]

vector_pushBack :: Exp -> Exp -> StmtM ()
vector_pushBack a b = tell [Vector_pushBack a b]

vector_new :: Type -> String -> StmtM ()
vector_new t n = tell [Vector_new t n]

allocClassVars :: StmtM ()
allocClassVars = tell [AllocClassVars]

allocNativeArray :: Type -> Exp -> StmtM ()
allocNativeArray t n = tell [AllocNativeArray t n]

copyToNativeArray :: Type -> Exp -> Exp -> StmtM ()
copyToNativeArray t dst src = tell [CopyToNativeArray t dst src]

for :: StmtM () -> Exp -> Exp -> StmtM () -> StmtM ()
for loopInitM cond exp stmtM = tell [For (execWriter loopInitM) cond exp (execWriter stmtM)]

(/) :: Exp -> Exp -> Exp
(/) = (:/)

(&&) :: Exp -> Exp -> Exp
(&&) = (:&&)

(==) :: Exp -> Exp -> Exp
a == b = a :== b

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

class ToExp a where
  toExp :: a -> Exp

instance ToExp GLConstant where 
  toExp a = GLConstant a

-- enum related
nsPatADT :: String -> String -> Pat
nsPatADT = NsPatADT

enumVal :: String -> String -> Exp
enumVal = EnumVal

enumADT :: String -> String -> Exp
enumADT = EnumADT
