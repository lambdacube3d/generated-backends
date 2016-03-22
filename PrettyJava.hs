{-# LANGUAGE LambdaCase #-}
module PrettyJava (prettyJava) where

import System.Directory
import System.FilePath
import Control.Monad.Writer
import Data.List
import Language hiding ((.))

{-
  = Procedure String [Arg] Type [Stmt]  -- static method in a sppecial class
  | ClassDef  String [ClassScope]       -- class
  | EnumDef   String [String]           -- enum in spearated file
  | StructDef String [StructDef]        -- class
-}

prettyJava :: String -> DefM () -> IO ()
prettyJava path defM = do
  let inc = unlines
          [ "package LambdaCube.GLES20;"
          , ""
          , "import android.opengl.GLES20;"
          , "import LambdaCube.IR.*;"
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
          StructDef a _ -> a
          _ -> error "prettyJava: internal error"
    writeFile (path </> name ++ ".java") $ inc ++ prettyDef def

prettyDef :: Def -> String
prettyDef = \case
  Procedure name args retType stmts -> concat
    [ prettyType retType ++ " " ++ name ++ "(" ++ intercalate ", " (map prettyArg args) ++ ") {\n"
    , unlines $ map (prettyStmt 1) stmts
    , "}\n\n"
    ]
  EnumDef name args -> unlines
    [ unwords $ ["enum", name, "{"]
    , intercalate ",\n" $ map (addIndentation 1) args
    , "};"
    , ""
    ]
  ClassDef className args -> "public class " ++ className ++ " {\n" ++ concatMap (prettyClassScope className 1) args ++ "}\n\n"
  StructDef structName args -> "public class " ++ structName ++ " {\n" ++ concatMap (prettyStructDef 1) args ++ "};\n\n"
  x -> ""

prettyClassScope className ind = \case
  Public args -> concatMap (prettyClassDef True className ind) args
  Private args -> concatMap (prettyClassDef False className ind) args

prettyClassDef isPublic className ind = \case
  Method isStatic name args retType stmts -> concat
    [ addIndentation ind $ (if isStatic then "static " else "") ++ visibility ++ prettyType retType ++ " " ++ name ++ "(" ++ intercalate ", " (map prettyArg args) ++ ") {\n"
    , unlines $ map (prettyStmt $ ind + 1) stmts
    , addIndentation ind "}\n\n"
    ]
  Constructor args stmts -> concat
    [ addIndentation ind $ visibility ++ className ++ "(" ++ intercalate ", " (map prettyArg args) ++ ") {\n"
    , unlines $ map (prettyStmt $ ind + 1) stmts
    , addIndentation ind "}\n\n"
    ]
  Destructor stmts -> concat
    [ addIndentation ind $ "protected void finalize() {\n"
    , unlines $ map (prettyStmt $ ind + 1) stmts
    , addIndentation ind "}\n\n"
    ]
  ClassVar t vars -> addIndentation ind $ visibility ++ prettyType t ++ " " ++ intercalate ", " vars ++ ";\n"
  ClassUnion args -> concatMap (addIndentation ind . (visibility ++) . (++ ";\n") . prettyArg) args -- TODO
  x -> error $ show x
 where
  visibility = if isPublic then "public " else "protected "

prettyStructDef ind = \case
  StructVar t vars -> addIndentation ind $ "public " ++ prettyType t ++ " " ++ intercalate ", " vars ++ ";\n"
  StructUnion args -> concatMap (addIndentation ind . ("public " ++) . (++ ";\n") . prettyArg) args -- TODO: remove union

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
  Enum n  -> n
  Const t -> prettyType t
  Ref t   -> prettyType t
  Ptr t   -> prettyType t
  SmartPtr t -> prettyType t
  Vector t -> "ArrayList<" ++ prettyType t ++ ">"
  Map k v -> "HashMap<" ++ prettyType k ++ "," ++ prettyType v ++ ">"
  String  -> "String"
  x -> error $ show x

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
  Case p [s@(Return{})]  -> "case " ++ prettyPat p ++ ": " ++ prettyStmt 0 s
  Case p [s] | isSimple s -> "case " ++ prettyPat p ++ ": " ++ prettyStmt 0 s ++ " break;"
  Case p s  -> "case " ++ prettyPat p ++ ": {\n" ++ unlines (map (prettyStmt $ ind + 1) s) ++ addIndentation (ind + 1) "break;\n" ++ addIndentation ind "}"
  Default s -> "default:\n" ++ unlines (map (prettyStmt $ ind + 1) s)

prettyPat = \case
  NsPat a b -> b
  NsPatADT a b -> b

prettyStmt ind = addIndentation ind . \case
  Switch e c -> "switch (" ++ prettyExp e ++ ") {\n" ++ concatMap ((++"\n") . prettyCase (ind + 1)) c ++ addIndentation ind "}"
  Return e -> "return " ++ prettyExp e ++ ";"
  Throw s -> "throw new Exception(" ++ show s ++ ");"
  Call a b -> prettyExp a ++ "(" ++ intercalate ", " (map prettyExp b) ++ ");"
  CallProc a b -> "Util." ++ prettyExp a ++ "(" ++ intercalate ", " (map prettyExp b) ++ ");"
  If a b c -> "if (" ++ prettyExp a ++ ") {\n" ++ concatMap ((++"\n") . prettyStmt (ind + 1)) b ++ addIndentation ind "}" ++
              if null c then "" else " else {\n" ++ concatMap ((++"\n") . prettyStmt (ind + 1)) c ++ addIndentation ind "}"
  VarDef t n -> prettyType t ++ " " ++ intercalate ", " n ++ ";"
  VarADTDef t c n e -> t ++ "." ++ c ++ "_ " ++ n ++ " = (" ++ t ++ "." ++ c ++ "_)" ++ prettyExp e ++ ";"
  VarAssignDef t n e -> prettyType t ++ " " ++ n ++ " = " ++ prettyExp e ++ ";"
  VarConstructor t n e -> prettyType t ++ " " ++ n ++ " = " ++ prettyExp e ++ ";"
  VarCharPtrFromString n e -> "const char* " ++ n ++ " = " ++ prettyExp e ++ ".c_str();"
  a := b -> prettyExp a ++ " = " ++ prettyExp b ++ ";"
  For [a] b c stmts -> "for (" ++ prettyStmt 0 a ++ " " ++ prettyExp b ++ "; " ++ prettyExp c ++ ") {\n" ++ unlines (map (prettyStmt $ ind + 1) stmts) ++ addIndentation ind "}"
  a :/= b -> prettyExp a ++ " /= " ++ prettyExp b ++ ";"
  a :|= b -> prettyExp a ++ " |= " ++ prettyExp b ++ ";"
  a :+= b -> prettyExp a ++ " += " ++ prettyExp b ++ ";"
  Map_insert m k v -> prettyExp m ++ ".put(" ++ prettyExp k ++ ", " ++ prettyExp v ++ ");"
  Map_foreach tk tv n e s -> "for (Map.Entry<" ++ prettyType tk ++ "," ++ prettyType tv ++ "> " ++ n ++ " : " ++ prettyExp e ++ ".entrySet()) {\n" ++ unlines (map (prettyStmt $ ind + 1) s) ++ addIndentation ind "}"
  For_range n a b s -> "for (int " ++ n ++ " = " ++ prettyExp a ++ "; " ++ n ++ " < " ++ prettyExp b ++ "; " ++ n ++ "++) {\n" ++ unlines (map (prettyStmt $ ind + 1) s) ++ addIndentation ind "}"
  Vector_foreach t n e s -> "for (" ++ prettyType t ++ " " ++ n ++ " : " ++ prettyExp e ++ ") {\n" ++ unlines (map (prettyStmt $ ind + 1) s) ++ addIndentation ind "}"
  Vector_pushBack a b -> prettyExp a ++ ".add(" ++ prettyExp b ++ ");"
  Vector_pushBackPtr a b -> prettyExp a ++ ".add(" ++ prettyExp b ++ ");"
  Break -> "break;"
  Continue -> "continue;"
  Inc e -> prettyExp e ++ "++;"
  x -> error $ show x

prettyExp = \case
  a :-> b -> prettyExp a ++ "." ++ prettyExp b
  a :. b  -> prettyExp a ++ "." ++ prettyExp b
  Var n   -> n
  EnumVal a b -> a ++ "." ++ b
  EnumADT a b -> a ++ ".Tag." ++ b
  Integer a   -> show a
  FloatLit a  -> show a
  Cast t e    -> "(" ++ prettyType t ++ ")" ++ prettyExp e
  Addr e      -> prettyExp e
  Deref e     -> prettyExp e
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
  CharPtrFromString e -> prettyExp e ++ ".c_str()"
  New t a -> "new " ++ prettyType t ++ "(" ++ intercalate "," (map prettyExp a) ++ ")"
  IteratorValue e -> prettyExp e ++ ".getValue()" -- used with foreach
  IteratorKey e -> prettyExp e ++ ".getKey()" -- used with foreach
  RecordValue a -> "{" ++ intercalate ", " ["." ++ n ++ " = " ++ prettyExp v | (n,v) <- a] ++ "}"
  NotNull e -> prettyExp e ++ "!= null"
  Not e -> "!" ++ prettyExp e
  Map_notElem a b -> "!" ++ prettyExp a ++ ".containsKey(" ++ prettyExp b ++ ")"
  Map_elem a b -> prettyExp a ++ ".containsKey(" ++ prettyExp b ++ ")"
  Vector_size a -> prettyExp a ++ ".size()"
  Vector_dataPtr a -> prettyExp a ++ ".data()"
  CallTypeConsructor t a -> prettyType t ++ "(" ++ prettyExp a ++ ")"
  GLConstant a -> "GLES20." ++ show a
  GLCommand a -> "GLES20.gl" ++ drop 2 (show a)
  x -> error $ show x
