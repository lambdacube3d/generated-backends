{-# LANGUAGE LambdaCase #-}
module PrettyCpp (prettyCpp,prettyHpp) where

import Control.Monad.Writer
import Data.List
import Language hiding ((.))

prettyCpp :: DefM () -> String
prettyCpp defM = inc ++ (concat $ map (prettyDef True) $ execWriter defM)
  where inc = unlines
          [ "#include <iostream>"
          -- , "#include <OpenGL/gl.h>"
          , "#include <OpenGLES/ES2/gl.h>"
          , "#include \"LambdaCube.hpp\"" -- TODO
          , ""
          ]

prettyHpp :: DefM () -> String
prettyHpp defM = inc ++ (concat $ map (prettyDef False) $ execWriter defM) ++ "#endif\n"
  where inc = unlines
          [ "#ifndef HEADER_LambdaCube_H"
          , "#define HEADER_LambdaCube_H"
          , ""
          , "#include \"IR.hpp\""
          , ""]

prettyDef :: Bool -> Def -> String
prettyDef isCpp = \case
  Procedure name args retType stmts | isCpp -> concat
    [ prettyType retType ++ " " ++ name ++ "(" ++ intercalate ", " (map prettyArg args) ++ ") {\n"
    , unlines $ map (prettyStmt [] 1) stmts
    , "}\n\n"
    ]
  EnumDef name args | Prelude.not isCpp -> unlines
    [ unwords $ ["enum class", name, "{"]
    , intercalate ",\n" $ map (addIndentation 1) args
    , "};"
    , ""
    ]
  ClassDef className args | Prelude.not isCpp -> "class " ++ className ++ " {\n" ++ concatMap (prettyClassScope args className isCpp 1) args ++ "};\n\n"
  ClassDef className args | isCpp -> concat $ map (prettyClassScope args className isCpp 0) args
  x -> ""

prettyClassScope classDefs className False ind = \case
  Public args -> addIndentation ind "public:\n" ++ concatMap (prettyClassDef classDefs className False $ ind + 1) args
  Private args -> addIndentation ind "private:\n" ++ concatMap (prettyClassDef classDefs className False $ ind + 1) args

prettyClassScope classDefs className True ind = \case
  Public args -> concat $ map (prettyClassDef classDefs className True $ ind) args
  Private args -> concat $ map (prettyClassDef classDefs className True $ ind) args

prettyClassDef classDefs className isCpp ind = \case
  Method isStatic name args retType stmts -> concat $ cut
    [ addIndentation ind $ prettyType retType ++ " " ++ classNS ++ name ++ "(" ++ intercalate ", " (map prettyArg args) ++ ")" ++ terminator
    , unlines $ map (prettyStmt classDefs $ ind + 1) stmts
    , addIndentation ind "}\n\n"
    ]
  Constructor args stmts -> concat $ cut
    [ addIndentation ind $ classNS ++ className ++ "(" ++ intercalate ", " (map prettyArg args) ++ ")" ++ terminator
    , unlines $ map (prettyStmt classDefs $ ind + 1) stmts
    , addIndentation ind "}\n\n"
    ]
  Destructor stmts -> concat $ cut
    [ addIndentation ind $ classNS ++ "~" ++ className ++ "()" ++ terminator
    , unlines $ map (prettyStmt classDefs $ ind + 1) stmts
    , addIndentation ind "}\n\n"
    ]
  ClassVar t vars -> if isCpp then "" else addIndentation ind $ prettyType t ++ " " ++ intercalate ", " vars ++ ";\n"
  x -> error $ "cpp - prettyClassDef" ++ show x
 where
  classNS = if isCpp then className ++ "::" else ""
  terminator = if isCpp then " {\n" else ";\n"
  cut = if isCpp then id else take 1

prettyArg (n :@ t) = unwords [prettyType t,n]

prettyType = \case
  Bool  -> "bool"
  Float -> "float"
  Int   -> "int32_t"
  Int8  -> "int8_t"
  Int16 -> "int16_t"
  Long  -> "int64_t"
  UInt  -> "uint32_t"
  UInt8 -> "uint8_t"
  UInt16-> "uint16_t"
  Void  -> "void"
  Class n -> "std::shared_ptr<" ++ n ++ ">"
  Builtin n -> n
  Enum n  -> "enum " ++ n
  Vector t -> "std::vector<" ++ prettyType t ++ ">"
  Map k v -> "std::map<" ++ prettyType k ++ "," ++ prettyType v ++ ">"
  String  -> "std::string"
  ADTEnum a -> "enum ::" ++ a ++ "::tag"
  ADTCons a b -> "std::shared_ptr<::data::" ++ b ++ ">"
  NativeArray t -> prettyType t ++ "*"
  NativeBuffer -> "const void*"
  x -> error $ "cpp - prettyType: " ++ show x

addIndentation ind s = concat (replicate ind "  ") ++ s

isSimple = \case
  Call{} -> True
  (:=){} -> True
  (:/=){} -> True
  (:|=){} -> True
  (:+=){} -> True
  Map_insert{} -> True
  Vector_pushBack{} -> True
  Inc {} -> True
  _ -> False

prettyCase ind = addIndentation ind . \case
  Case p [s@(Return{})]  -> "case " ++ prettyPat p ++ ": " ++ prettyStmt [] 0 s
  Case p [s] | isSimple s -> "case " ++ prettyPat p ++ ": " ++ prettyStmt [] 0 s ++ " break;"
  Case p s  -> "case " ++ prettyPat p ++ ": {\n" ++ unlines (map (prettyStmt [] $ ind + 1) s) ++ addIndentation (ind + 1) "break;\n" ++ addIndentation ind "}"
  Default s -> "default:\n" ++ unlines (map (prettyStmt [] $ ind + 1) s)

prettyPat = \case
  NsPat a b -> "::" ++ a ++ "::" ++ b
  NsPatADT a b -> "::" ++ a ++ "::tag::" ++ b
  GLPat a -> show a

prettyStmt classDefs ind = addIndentation ind . \case
  Switch e c -> "switch (" ++ prettyExp e ++ ") {\n" ++ concatMap ((++"\n") . prettyCase (ind + 1)) c ++ addIndentation ind "}"
  Return e -> "return " ++ prettyExp e ++ ";"
  Throw s -> "throw " ++ show s ++ ";"
  CallGLPrim prim -> prettyGLPrim prim
  Call a b -> prettyExp a ++ "(" ++ intercalate ", " (map prettyExp b) ++ ");"
  CallProc a b -> prettyExp a ++ "(" ++ intercalate ", " (map prettyExp b) ++ ");"
  If a b c -> "if (" ++ prettyExp a ++ ") {\n" ++ concatMap ((++"\n") . prettyStmt [] (ind + 1)) b ++ addIndentation ind "}" ++
              if null c then "" else " else {\n" ++ concatMap ((++"\n") . prettyStmt [] (ind + 1)) c ++ addIndentation ind "}"
  VarDef t n -> prettyType t ++ " " ++ intercalate ", " n ++ ";"
  VarADTDef t c n e -> "auto " ++ n ++ " = std::static_pointer_cast<data::" ++ c ++ ">(" ++ prettyExp e ++ ");"
  VarAssignDef t n e -> prettyType t ++ " " ++ n ++ " = " ++ prettyExp e ++ ";"
  VarConstructor t n e -> prettyType (Class t) ++ " " ++ n ++ "(new " ++ t ++ "(" ++ intercalate ", " (map prettyExp e) ++ "));"
  VarRecordValue t n a -> prettyType t ++ " " ++ n ++ " = " ++ "{" ++ intercalate ", " ["." ++ n ++ " = " ++ prettyExp v | (n,v) <- a] ++ "}"
  VarNativeBufferFrom t n a -> "void* " ++ n ++ " = " ++ prettyExp a ++ ".data();"
  a := b -> prettyExp a ++ " = " ++ prettyExp b ++ ";"
  For [a] b c stmts -> "for (" ++ prettyStmt [] 0 a ++ " " ++ prettyExp b ++ "; " ++ prettyExp c ++ ") {\n" ++ unlines (map (prettyStmt [] $ ind + 1) stmts) ++ addIndentation ind "}"
  a :/= b -> prettyExp a ++ " /= " ++ prettyExp b ++ ";"
  a :|= b -> prettyExp a ++ " |= " ++ prettyExp b ++ ";"
  a :+= b -> prettyExp a ++ " += " ++ prettyExp b ++ ";"
  Map_insert m k v -> prettyExp m ++ "[" ++ prettyExp k ++ "] = " ++ prettyExp v ++ ";"
  Map_foreach tk tv n e s -> "for (auto " ++ n ++ " : " ++ prettyExp e ++ ") {\n" ++ unlines (map (prettyStmt [] $ ind + 1) s) ++ addIndentation ind "}"
  For_range n a b s -> "for (int " ++ n ++ " = " ++ prettyExp a ++ "; " ++ n ++ " < " ++ prettyExp b ++ "; " ++ n ++ "++) {\n" ++ unlines (map (prettyStmt [] $ ind + 1) s) ++ addIndentation ind "}"
  Vector_foreach t n e s -> "for (auto " ++ n ++ " : " ++ prettyExp e ++ ") {\n" ++ unlines (map (prettyStmt [] $ ind + 1) s) ++ addIndentation ind "}"
  Vector_pushBack a b -> prettyExp a ++ ".push_back(" ++ prettyExp b ++ ");"
  Vector_new t n -> prettyType (Vector t) ++ " " ++ n ++ ";"
  Break -> "break;"
  Continue -> "continue;"
  Inc e -> prettyExp e ++ "++;"
  AllocClassVars -> "" -- TODO
  --AllocNativeArray t n -> "" -- TODO
  --CopyToNativeArray t dst src -> "" -- TODO
  AllocNativeArray t n -> prettyExp n ++ " = new " ++ case t of
    Int   -> "int[1];"
    Bool  -> "int[1];"
    Float -> "float[1];"
    (Builtin "V2I") -> "int[2];"
    (Builtin "V2B") -> "int[2];"
    (Builtin "V2F") -> "float[2];"
    (Builtin "V3I") -> "int[3];"
    (Builtin "V3B") -> "int[3];"
    (Builtin "V3F") -> "float[3];"
    (Builtin "V4I") -> "int[4];"
    (Builtin "V4B") -> "int[4];"
    (Builtin "V4F") -> "float[4];"
    (Builtin "M22F")  -> "float[4];"
    (Builtin "M33F")  -> "float[9];"
    (Builtin "M44F")  -> "float[16];"
  CopyToNativeArray t dst src -> case t of
      Int   -> f 0 ""
      Bool  -> f 0 "?1:0"
      Float -> f 0 ""
      (Builtin "V2I") -> intercalate indStr [f 0 ".x", f 1 ".y"]
      (Builtin "V2B") -> intercalate indStr [f 0 ".x?1:0", f 1 ".y?1:0"]
      (Builtin "V2F") -> intercalate indStr [f 0 ".x", f 1 ".y"]
      (Builtin "V3I") -> intercalate indStr [f 0 ".x", f 1 ".y", f 2 ".z"]
      (Builtin "V3B") -> intercalate indStr [f 0 ".x?1:0", f 1 ".y?1:0", f 2 ".z?1:0"]
      (Builtin "V3F") -> intercalate indStr [f 0 ".x", f 1 ".y", f 2 ".z"]
      (Builtin "V4I") -> intercalate indStr [f 0 ".x", f 1 ".y", f 2 ".z", f 3 ".w"]
      (Builtin "V4B") -> intercalate indStr [f 0 ".x?1:0", f 1 ".y?1:0", f 2 ".z?1:0", f 3 ".w?1:0"]
      (Builtin "V4F") -> intercalate indStr [f 0 ".x", f 1 ".y", f 2 ".z", f 3 ".w"]
      (Builtin "M22F")  -> intercalate indStr [ f 0 ".x.x", f 1 ".x.y"
                         , f 2 ".y.x", f 3 ".y.y"
                         ]
      (Builtin "M33F")  -> intercalate indStr [ f 0 ".x.x", f 1 ".x.y", f 2 ".x.z"
                         , f 3 ".y.x", f 4 ".y.y", f 5 ".y.z"
                         , f 6 ".z.x", f 7 ".z.y", f 8 ".z.z"
                         ]
      (Builtin "M44F")  -> intercalate indStr [ f 0 ".x.x", f 1 ".x.y", f 2 ".x.z", f 3 ".x.w"
                         , f 4 ".y.x", f 5 ".y.y", f 6 ".y.z", f 7 ".y.w"
                         , f 8 ".z.x", f 9 ".z.y", f 10 ".z.z", f 11 ".z.w"
                         , f 12 ".w.x", f 13 ".w.y", f 14 ".w.z", f 15 ".w.w"
                         ]
    where
      indStr = "\n" ++ addIndentation ind ""
      f i n = prettyExp dst ++ "["  ++ show i ++ "] = " ++ prettyExp src ++ n ++ ";"
  x -> error $ "cpp - prettyStmt: " ++ show x

prettyExp = \case
  a :-> b -> prettyExp a ++ "->" ++ prettyExp b
  a :. b  -> prettyExp a ++ "." ++ prettyExp b
  Var n   -> n
  EnumVal a b -> "::" ++ a ++ "::" ++ b
  EnumADT a b -> "::" ++ a ++ "::tag::" ++ b
  Integer a   -> show a
  FloatLit a  -> show a
  BoolLit True  -> "true"
  BoolLit False -> "false"
  Vector_lookup a b -> prettyExp a ++ "[" ++ prettyExp b ++ "]"
  Map_lookup a b    -> prettyExp a ++ "[" ++ prettyExp b ++ "]"
  a :+ b  -> prettyExp a ++ " + " ++ prettyExp b
  a :- b  -> prettyExp a ++ " - " ++ prettyExp b
  a :* b  -> prettyExp a ++ " * " ++ prettyExp b
  a :/ b  -> prettyExp a ++ " / " ++ prettyExp b
  a :!= b -> prettyExp a ++ " != " ++ prettyExp b
  a :== b -> prettyExp a ++ " == " ++ prettyExp b
  a :<= b -> prettyExp a ++ " <= " ++ prettyExp b
  a :>= b -> prettyExp a ++ " >= " ++ prettyExp b
  a :&& b -> prettyExp a ++ " && " ++ prettyExp b
  IncExp a -> prettyExp a ++ "++ "
  CallExp n a -> prettyExp n ++ "(" ++ intercalate ", " (map prettyExp a) ++ ")"
  CallProcExp n a -> prettyExp n ++ "(" ++ intercalate ", " (map prettyExp a) ++ ")"
  ExpIf a b c -> prettyExp a ++ "?" ++ prettyExp b ++ ":" ++ prettyExp c
  NullPtr -> "nullptr"
  IteratorValue e -> prettyExp e ++ ".second" -- used with foreach
  IteratorKey e -> prettyExp e ++ ".first" -- used with foreach
  NotNull e -> prettyExp e -- HACK
  Not e -> "!" ++ prettyExp e
  Map_notElem a b -> prettyExp a ++ ".count(" ++ prettyExp b ++ ")<=0"
  Map_elem a b -> prettyExp a ++ ".count(" ++ prettyExp b ++ ")>0"
  Vector_size a -> prettyExp a ++ ".size()"
  GLConstant a -> show a
  GLCommand a -> "gl" ++ drop 2 (show a)
  x -> error $ "cpp - prettyExp: " ++ show x

prettyGLPrim = \case
  GLGenTexture e -> "{ unsigned int glObj; glGenTextures(1, &glObj); " ++ prettyExp e ++ " = glObj; }"
  GLGenFramebuffer e -> "{ unsigned int glObj; glGenFramebuffers(1, &glObj); " ++ prettyExp e ++ " = glObj; }"
  GLGenBuffer e -> "{ unsigned int glObj; glGenBuffers(1, &glObj); " ++ prettyExp e ++ " = glObj; }"
  GLDeleteTexture e -> "{ unsigned int glObj = " ++ prettyExp e ++ "; glDeleteTextures(1, &glObj); }"
  GLDeleteFramebuffer e -> "{ unsigned int glObj = " ++ prettyExp e ++ "; glDeleteFramebuffers(1, &glObj); }"
  GLShaderSource vs src -> "{ const char* glslSrc = " ++ prettyExp src ++ ".c_str(); glShaderSource(" ++ prettyExp vs ++ ", 1, &glslSrc, 0); }"
  GLGetUniformLocation p n v -> prettyExp v ++ " = glGetUniformLocation(" ++ prettyExp p ++ ", " ++ prettyExp n ++ ".c_str());"
  GLGetAttribLocation p n v -> prettyExp v ++ " = glGetAttribLocation(" ++ prettyExp p ++ ", " ++ prettyExp n ++ ".c_str());"
  GLUniform1iv a b c -> "glUniform1iv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ");"
  GLUniform1fv a b c -> "glUniform1fv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ");"
  GLUniform2iv a b c -> "glUniform2iv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ");"
  GLUniform2fv a b c -> "glUniform2fv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ");"
  GLUniform3iv a b c -> "glUniform3iv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ");"
  GLUniform3fv a b c -> "glUniform3fv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ");"
  GLUniform4iv a b c -> "glUniform4iv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ");"
  GLUniform4fv a b c -> "glUniform4fv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ");"
  GLUniformMatrix2fv a b c d -> "glUniformMatrix2fv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ", " ++ prettyExp d ++ ");"
  GLUniformMatrix3fv a b c d -> "glUniformMatrix3fv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ", " ++ prettyExp d ++ ");"
  GLUniformMatrix4fv a b c d -> "glUniformMatrix4fv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ", " ++ prettyExp d ++ ");"
  GLVertexAttribPointer a b c d e f -> "glVertexAttribPointer(" ++ intercalate ", " (map prettyExp [a,b,c,d,e]) ++ ", (const void*) " ++ prettyExp f ++ ");"
  GLVertexAttrib1fv a b c -> "glVertexAttrib1fv(" ++ intercalate ", " (map prettyExp [a,b]) ++ " + " ++ prettyExp c ++ ");"
  GLVertexAttrib2fv a b c -> "glVertexAttrib2fv(" ++ intercalate ", " (map prettyExp [a,b]) ++ " + " ++ prettyExp c ++ ");"
  GLVertexAttrib3fv a b c -> "glVertexAttrib3fv(" ++ intercalate ", " (map prettyExp [a,b]) ++ " + " ++ prettyExp c ++ ");"
  GLVertexAttrib4fv a b c -> "glVertexAttrib4fv(" ++ intercalate ", " (map prettyExp [a,b]) ++ " + " ++ prettyExp c ++ ");"
  x -> error $ "cpp - prettyGLPrim: " ++ show x
