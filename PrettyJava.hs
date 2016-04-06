{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleContexts #-}
module PrettyJava (prettyJava) where

import Data.Maybe
import System.Directory
import System.FilePath
import Control.Monad.Writer
import Data.List
import Language hiding ((.))

prettyJava :: String -> DefM () -> IO ()
prettyJava path defM = do
  let inc = unlines
          [ "package LambdaCube.GLES20;"
          , ""
          , "import java.nio.*;"
          , "import android.opengl.GLES20;"
          , "import LambdaCube.IR.*;"
          , "import RT.*;"
          , "import java.util.ArrayList;"
          , "import java.util.HashMap;"
          , "import java.util.Map;"
          , ""
          ]
      isProcedure Procedure{} = True
      isProcedure _ = False
      (procs,others) = partition isProcedure $ execWriter defM
      util = ClassDef "Util" [Public [Method True a b c d | Procedure a b c d <- procs]]
  createDirectoryIfMissing True path

  forM_ (util:others) $ \def -> do
    let name = case def of
          ClassDef a _ -> a
          EnumDef a _ -> a
          _ -> error "prettyJava: internal error"
    writeFile (path </> name ++ ".java") $ inc ++ prettyDef def

prettyDef :: Def -> String
prettyDef = \case
  Procedure name args retType stmts -> concat
    [ prettyType retType ++ " " ++ name ++ "(" ++ intercalate ", " (map prettyArg args) ++ ") {\n"
    , unlines $ map (prettyStmt [] 1) stmts
    , "}\n\n"
    ]
  EnumDef name args -> unlines
    [ unwords $ ["enum", name, "{"]
    , intercalate ",\n" $ map (addIndentation 1) args
    , "};"
    , ""
    ]
  ClassDef className args -> "public class " ++ className ++ " {\n" ++ concatMap (prettyClassScope args className 1) args ++ "}\n\n"
  x -> ""

prettyClassScope classDefs className ind = \case
  Public args -> concatMap (prettyClassDef classDefs True className ind) args
  Private args -> concatMap (prettyClassDef classDefs False className ind) args

prettyClassDef classDefs isPublic className ind = \case
  Method isStatic name args retType stmts -> concat
    [ addIndentation ind $ (if isStatic then "static " else "") ++ visibility ++ prettyType retType ++ " " ++ name ++ "(" ++ intercalate ", " (map prettyArg args) ++ ") throws Exception {\n"
    , unlines $ map (prettyStmt classDefs $ ind + 1) stmts
    , addIndentation ind "}\n\n"
    ]
  Constructor args stmts -> concat
    [ addIndentation ind $ visibility ++ className ++ "(" ++ intercalate ", " (map prettyArg args) ++ ") throws Exception {\n"
    , unlines $ map (prettyStmt classDefs $ ind + 1) stmts
    , addIndentation ind "}\n\n"
    ]
  Destructor stmts -> concat
    [ addIndentation ind $ "protected void finalize() {\n"
    , unlines $ map (prettyStmt classDefs $ ind + 1) stmts
    , addIndentation ind "}\n\n"
    ]
  ClassVar t vars -> addIndentation ind $ visibility ++ prettyType t ++ " " ++ intercalate ", " vars ++ ";\n"
  x -> error $ "java - prettyClassDef: " ++ show x
 where
  visibility = if isPublic then "public " else "protected "

prettyArg (n :@ t) = unwords [prettyType t,n]

prettyType = \case
  Bool  -> "Boolean"
  Float -> "Float"
  Int   -> "Integer"
  Int8  -> "Integer"
  Int16 -> "Integer"
  Long  -> "Integer"
  UInt  -> "Integer"
  UInt8 -> "Integer"
  UInt16-> "Integer"
  Void  -> "void"
  Class n -> n
  Builtin n -> n
  Enum n  -> n
  Vector t -> "ArrayList<" ++ prettyType t ++ ">"
  Map k v -> "HashMap<" ++ prettyType k ++ "," ++ prettyType v ++ ">"
  String  -> "String"
  ADTEnum a -> a ++ ".Tag"
  ADTCons a b -> a ++ "." ++ b ++ "_"
  NativeArray Int -> "int[]"
  NativeArray Float -> "float[]"
  NativeBuffer -> "Buffer"
  x -> error $ "java - prettyType: " ++ show x

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
  NsPat a b -> b
  NsPatADT a b -> b
  GLPat a -> "GLES20." ++ show a

prettyStmt classDefs ind = addIndentation ind . \case
  Switch e c -> "switch (" ++ prettyExp e ++ ") {\n" ++ concatMap ((++"\n") . prettyCase (ind + 1)) c ++ addIndentation ind "}"
  Return e -> "return " ++ prettyExp e ++ ";"
  Throw s -> "throw new Exception(" ++ show s ++ ");"
  CallGLPrim prim -> prettyGLPrim prim
  Call a b -> prettyExp a ++ "(" ++ intercalate ", " (map prettyExp b) ++ ");"
  CallProc a b -> "Util." ++ prettyExp a ++ "(" ++ intercalate ", " (map prettyExp b) ++ ");"
  If a b c -> "if (" ++ prettyExp a ++ ") {\n" ++ concatMap ((++"\n") . prettyStmt classDefs (ind + 1)) b ++ addIndentation ind "}" ++
              if null c then "" else " else {\n" ++ concatMap ((++"\n") . prettyStmt classDefs (ind + 1)) c ++ addIndentation ind "}"
  VarDef t n -> prettyType t ++ " " ++ intercalate ", " n ++ ";"
  VarADTDef t c n e -> t ++ "." ++ c ++ "_ " ++ n ++ " = (" ++ t ++ "." ++ c ++ "_)" ++ prettyExp e ++ ";"
  VarAssignDef t n e -> prettyType t ++ " " ++ n ++ " = " ++ prettyExp e ++ ";"
  VarConstructor t n e -> prettyType (Class t) ++ " " ++ n ++ " = new " ++ t ++ "(" ++ intercalate ", " (map prettyExp e) ++ ");"
  VarRecordValue t x a -> prettyType t ++ " " ++ x ++ " = new " ++ prettyType t ++ "();\n" ++ intercalate "\n" [addIndentation ind $ x ++ "." ++ n ++ " = " ++ prettyExp v ++ ";" | (n,v) <- a]
  VarNativeBufferFrom t n a -> let tStr = case t of
                                    Vector Float -> "FloatBuffer"
                                   populate = "for (Float vec_elem : " ++ prettyExp a ++ ") " ++ n ++ ".put(vec_elem);\n" ++ addIndentation ind (n ++ ".rewind();")
                               in tStr ++ " " ++ n ++ " = " ++ tStr ++ ".allocate(" ++ prettyExp a ++ ".size());\n" ++ addIndentation ind populate
  a := b -> prettyExp a ++ " = " ++ prettyExp b ++ ";"
  For [a] b c stmts -> "for (" ++ prettyStmt classDefs 0 a ++ " " ++ prettyExp b ++ "; " ++ prettyExp c ++ ") {\n" ++ unlines (map (prettyStmt classDefs $ ind + 1) stmts) ++ addIndentation ind "}"
  a :/= b -> prettyExp a ++ " /= " ++ prettyExp b ++ ";"
  a :|= b -> prettyExp a ++ " |= " ++ prettyExp b ++ ";"
  a :+= b -> prettyExp a ++ " += " ++ prettyExp b ++ ";"
  Map_insert m k v -> prettyExp m ++ ".put(" ++ prettyExp k ++ ", " ++ prettyExp v ++ ");"
  Map_foreach tk tv n e s -> "for (Map.Entry<" ++ prettyType tk ++ "," ++ prettyType tv ++ "> " ++ n ++ " : " ++ prettyExp e ++ ".entrySet()) {\n" ++ unlines (map (prettyStmt classDefs $ ind + 1) s) ++ addIndentation ind "}"
  For_range n a b s -> "for (int " ++ n ++ " = " ++ prettyExp a ++ "; " ++ n ++ " < " ++ prettyExp b ++ "; " ++ n ++ "++) {\n" ++ unlines (map (prettyStmt classDefs $ ind + 1) s) ++ addIndentation ind "}"
  Vector_foreach t n e s -> "for (" ++ prettyType t ++ " " ++ n ++ " : " ++ prettyExp e ++ ") {\n" ++ unlines (map (prettyStmt classDefs $ ind + 1) s) ++ addIndentation ind "}"
  Vector_pushBack a b -> prettyExp a ++ ".add(" ++ prettyExp b ++ ");"
  Vector_new t n -> prettyType (Vector t) ++ " " ++ n ++ " = new " ++ prettyType (Vector t) ++ "();"
  Break -> "break;"
  Continue -> "continue;"
  Inc e -> prettyExp e ++ "++;"
  AllocClassVars -> let vars = catMaybes . map filterVar $ [a | Public l <- classDefs, a@ClassVar{} <- l] ++ [a | Private l <- classDefs, a@ClassVar{} <- l]
                        filterVar (ClassVar t l) = case t of
                          Class{} -> Just $ map (allocVar t) l
                          Vector{} -> Just $ map (allocVar t) l
                          Map{} -> Just $ map (allocVar t) l
                          _ -> Nothing
                        allocVar t n = n ++ " = new " ++ prettyType t ++ "();"
                    in "\n" ++ (unlines $ map (addIndentation ind) $ concat vars)
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
  x -> error $ "java - prettyStmt: " ++ show x

prettyExp = \case
  a :-> b -> prettyExp a ++ "." ++ prettyExp b
  a :. b  -> prettyExp a ++ "." ++ prettyExp b
  Var n   -> n
  EnumVal a b -> a ++ "." ++ b
  EnumADT a b -> a ++ ".Tag." ++ b
  Integer a   -> show a
  FloatLit a  -> show a ++ "f"
  BoolLit True  -> "true"
  BoolLit False -> "false"
  Vector_lookup a b -> prettyExp a ++ ".get(" ++ prettyExp b ++ ")"
  Map_lookup a b    -> prettyExp a ++ ".get(" ++ prettyExp b ++ ")"
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
  CallProcExp n a -> "Util." ++ prettyExp n ++ "(" ++ intercalate ", " (map prettyExp a) ++ ")"
  ExpIf a b c -> prettyExp a ++ "?" ++ prettyExp b ++ ":" ++ prettyExp c
  NullPtr -> "null"
  IteratorValue e -> prettyExp e ++ ".getValue()" -- used with foreach
  IteratorKey e -> prettyExp e ++ ".getKey()" -- used with foreach
  NotNull e -> prettyExp e ++ "!= null"
  Not e -> "!" ++ prettyExp e
  Map_notElem a b -> "!" ++ prettyExp a ++ ".containsKey(" ++ prettyExp b ++ ")"
  Map_elem a b -> prettyExp a ++ ".containsKey(" ++ prettyExp b ++ ")"
  Vector_size a -> prettyExp a ++ ".size()"
  GLConstant a -> "GLES20." ++ show a
  GLCommand a -> "GLES20.gl" ++ drop 2 (show a)
  x -> error $ "java - prettyExp: " ++ show x

prettyGLPrim = \case
  GLGenTexture e -> "{ int[] glObj = new int[1]; GLES20.glGenTextures(1, glObj, 0); " ++ prettyExp e ++ " = glObj[0]; }"
  GLGenFramebuffer e -> "{ int[] glObj = new int[1]; GLES20.glGenFramebuffers(1, glObj, 0); " ++ prettyExp e ++ " = glObj[0]; }"
  GLGenBuffer e -> "{ int[] glObj = new int[1]; GLES20.glGenBuffers(1, glObj, 0); " ++ prettyExp e ++ " = glObj[0]; }"
  GLDeleteTexture e -> "{ int[] glObj = new int[1]; glObj[0] = " ++ prettyExp e ++ "; GLES20.glDeleteTextures(1, glObj, 0);}"
  GLDeleteFramebuffer e -> "{ int[] glObj = new int[1]; glObj[0] = " ++ prettyExp e ++ "; GLES20.glDeleteFramebuffers(1, glObj, 0);}"
  GLShaderSource vs src -> "GLES20.glShaderSource(" ++ prettyExp vs ++ ", " ++ prettyExp src ++ ");"
  GLGetUniformLocation p n v -> prettyExp v ++ " = GLES20.glGetUniformLocation(" ++ prettyExp p ++ ", " ++ prettyExp n ++ ");"
  GLGetAttribLocation p n v -> prettyExp v ++ " = GLES20.glGetAttribLocation(" ++ prettyExp p ++ ", " ++ prettyExp n ++ ");"
  GLUniform1iv a b c -> "GLES20.glUniform1iv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ", 0);"
  GLUniform1fv a b c -> "GLES20.glUniform1fv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ", 0);"
  GLUniform2iv a b c -> "GLES20.glUniform2iv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ", 0);"
  GLUniform2fv a b c -> "GLES20.glUniform2fv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ", 0);"
  GLUniform3iv a b c -> "GLES20.glUniform3iv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ", 0);"
  GLUniform3fv a b c -> "GLES20.glUniform3fv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ", 0);"
  GLUniform4iv a b c -> "GLES20.glUniform4iv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ", 0);"
  GLUniform4fv a b c -> "GLES20.glUniform4fv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ", 0);"
  GLUniformMatrix2fv a b c d -> "GLES20.glUniformMatrix2fv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ", " ++ prettyExp d ++ ", 0);"
  GLUniformMatrix3fv a b c d -> "GLES20.glUniformMatrix3fv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ", " ++ prettyExp d ++ ", 0);"
  GLUniformMatrix4fv a b c d -> "GLES20.glUniformMatrix4fv(" ++ prettyExp a ++ ", " ++ prettyExp b ++ ", " ++ prettyExp c ++ ", " ++ prettyExp d ++ ", 0);"
  GLVertexAttribPointer a b c d e f -> "GLES20.glVertexAttribPointer(" ++ intercalate ", " (map prettyExp [a,b,c,d,e,f]) ++ ");"
  GLVertexAttrib1fv a b c -> "GLES20.glVertexAttrib1fv(" ++ intercalate ", " (map prettyExp [a,b,c]) ++ ");"
  GLVertexAttrib2fv a b c -> "GLES20.glVertexAttrib2fv(" ++ intercalate ", " (map prettyExp [a,b,c]) ++ ");"
  GLVertexAttrib3fv a b c -> "GLES20.glVertexAttrib3fv(" ++ intercalate ", " (map prettyExp [a,b,c]) ++ ");"
  GLVertexAttrib4fv a b c -> "GLES20.glVertexAttrib4fv(" ++ intercalate ", " (map prettyExp [a,b,c]) ++ ");"
  x -> error $ "java - prettyGLPrim: " ++ show x
