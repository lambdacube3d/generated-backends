{-# LANGUAGE LambdaCase #-}
module PrettyCpp where

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
    , unlines $ map (prettyStmt 1) stmts
    , "}\n\n"
    ]
  EnumDef name args | Prelude.not isCpp -> unlines
    [ unwords $ ["enum class", name, "{"]
    , intercalate ",\n" $ map (addIndentation 1) args
    , "};"
    , ""
    ]
  ClassDef className args | Prelude.not isCpp -> "class " ++ className ++ " {\n" ++ concatMap (prettyClassScope className isCpp 1) args ++ "};\n\n"
  ClassDef className args | isCpp -> concat $ map (prettyClassScope className isCpp 0) args
  StructDef structName args | Prelude.not isCpp -> "struct " ++ structName ++ " {\n" ++ concatMap (prettyStructDef 1) args ++ "};\n\n"
  x -> ""

prettyClassScope className False ind = \case
  Public args -> addIndentation ind "public:\n" ++ concatMap (prettyClassDef className False $ ind + 1) args
  Private args -> addIndentation ind "private:\n" ++ concatMap (prettyClassDef className False $ ind + 1) args

prettyClassScope className True ind = \case
  Public args -> concat $ map (prettyClassDef className True $ ind) args
  Private args -> concat $ map (prettyClassDef className True $ ind) args

prettyClassDef className isCpp ind = \case
  Method name args retType stmts -> concat $ cut
    [ addIndentation ind $ prettyType retType ++ " " ++ classNS ++ name ++ "(" ++ intercalate ", " (map prettyArg args) ++ ")" ++ terminator
    , unlines $ map (prettyStmt $ ind + 1) stmts
    , addIndentation ind "}\n\n"
    ]
  Constructor args stmts -> concat $ cut
    [ addIndentation ind $ classNS ++ className ++ "(" ++ intercalate ", " (map prettyArg args) ++ ")" ++ terminator
    , unlines $ map (prettyStmt $ ind + 1) stmts
    , addIndentation ind "}\n\n"
    ]
  Destructor stmts -> concat $ cut
    [ addIndentation ind $ classNS ++ "~" ++ className ++ "()" ++ terminator
    , unlines $ map (prettyStmt $ ind + 1) stmts
    , addIndentation ind "}\n\n"
    ]
  ClassVar t vars -> if isCpp then "" else addIndentation ind $ prettyType t ++ " " ++ intercalate ", " vars ++ ";\n"
  ClassUnion args -> if isCpp then "" else addIndentation ind "union {\n" ++ concatMap (addIndentation (ind + 1) . (++ ";\n") . prettyArg) args ++ addIndentation ind "};\n"
  x -> error $ show x
 where
  classNS = if isCpp then className ++ "::" else ""
  terminator = if isCpp then " {\n" else ";\n"
  cut = if isCpp then id else take 1

prettyStructDef ind = \case
  StructVar t vars -> addIndentation ind $ prettyType t ++ " " ++ intercalate ", " vars ++ ";\n"
  StructUnion args -> addIndentation ind "union {\n" ++ concatMap (addIndentation (ind + 1) . (++ ";\n") . prettyArg) args ++ addIndentation ind "};\n"

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
  Class n -> n
  Enum n  -> "enum " ++ n
  Const t -> "const " ++ prettyType t
  Ref t   -> prettyType t ++ "&"
  Ptr t   -> prettyType t ++ "*"
  SmartPtr t -> "std::shared_ptr<" ++ prettyType t ++ ">"
  Vector t -> "std::vector<" ++ prettyType t ++ ">"
  Map k v -> "std::map<" ++ prettyType k ++ "," ++ prettyType v ++ ">"
  String  -> "std::string"
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
  NsPat a -> "::" ++ intercalate "::" a

prettyStmt ind = addIndentation ind . \case
  Switch e c -> "switch (" ++ prettyExp e ++ ") {\n" ++ concatMap ((++"\n") . prettyCase (ind + 1)) c ++ addIndentation ind "}"
  Return e -> "return " ++ prettyExp e ++ ";"
  Throw s -> "throw " ++ show s ++ ";"
  Call a b -> prettyExp a ++ "(" ++ intercalate ", " (map prettyExp b) ++ ");"
  If a b c -> "if (" ++ prettyExp a ++ ") {\n" ++ concatMap ((++"\n") . prettyStmt (ind + 1)) b ++ addIndentation ind "}" ++
              if null c then "" else " else {\n" ++ concatMap ((++"\n") . prettyStmt (ind + 1)) c ++ addIndentation ind "}"
  VarDef t n -> prettyType t ++ " " ++ intercalate ", " n ++ ";"
  VarADTDef t n e -> "auto " ++ n ++ " = std::static_pointer_cast<data::" ++ t ++ ">(" ++ prettyExp e ++ ");"
  VarAssignDef t n e -> prettyType t ++ " " ++ n ++ " = " ++ prettyExp e ++ ";"
  VarConstructor t n e -> prettyType t ++ " " ++ n ++ "(" ++ prettyExp e ++ ");"
  VarCharPtrFromString n e -> "const char* " ++ n ++ " = " ++ prettyExp e ++ ".c_str();"
  a := b -> prettyExp a ++ " = " ++ prettyExp b ++ ";"
  For [a] b c stmts -> "for (" ++ prettyStmt 0 a ++ " " ++ prettyExp b ++ "; " ++ prettyExp c ++ ") {\n" ++ unlines (map (prettyStmt $ ind + 1) stmts) ++ addIndentation ind "}"
  a :/= b -> prettyExp a ++ " /= " ++ prettyExp b ++ ";"
  a :|= b -> prettyExp a ++ " |= " ++ prettyExp b ++ ";"
  a :+= b -> prettyExp a ++ " += " ++ prettyExp b ++ ";"
  Map_insert m k v -> prettyExp m ++ "[" ++ prettyExp k ++ "] = " ++ prettyExp v ++ ";"
  Map_foreach n e s -> "for (auto " ++ n ++ " : " ++ prettyExp e ++ ") {\n" ++ unlines (map (prettyStmt $ ind + 1) s) ++ addIndentation ind "}"
  For_range n a b s -> "for (int " ++ n ++ " = " ++ prettyExp a ++ "; " ++ n ++ " < " ++ prettyExp b ++ "; " ++ n ++ "++) {\n" ++ unlines (map (prettyStmt $ ind + 1) s) ++ addIndentation ind "}"
  Vector_foreach n e s -> "for (auto " ++ n ++ " : " ++ prettyExp e ++ ") {\n" ++ unlines (map (prettyStmt $ ind + 1) s) ++ addIndentation ind "}"
  Vector_pushBack a b -> prettyExp a ++ ".push_back(" ++ prettyExp b ++ ");"
  Vector_pushBackPtr a b -> prettyExp a ++ "->push_back(" ++ prettyExp b ++ ");"
  Break -> "break;"
  Continue -> "continue;"
  Inc e -> prettyExp e ++ "++;"
  x -> error $ show x

prettyExp = \case
  a :-> b -> prettyExp a ++ "->" ++ prettyExp b
  a :. b  -> prettyExp a ++ "." ++ prettyExp b
  Var n   -> n
  Ns a    -> "::" ++ intercalate "::" a
  Integer a   -> show a
  FloatLit a  -> show a
  Cast t e    -> "(" ++ prettyType t ++ ")" ++ prettyExp e
  Addr e      -> "&" ++ prettyExp e
  Deref e     -> "*" ++ prettyExp e
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
  ExpIf a b c -> prettyExp a ++ "?" ++ prettyExp b ++ ":" ++ prettyExp c
  NullPtr -> "nullptr"
  CharPtrFromString e -> prettyExp e ++ ".c_str()"
  New t a -> "new " ++ prettyType t ++ "(" ++ intercalate "," (map prettyExp a) ++ ")"
  IteratorValue e -> prettyExp e ++ ".second" -- used with foreach
  IteratorKey e -> prettyExp e ++ ".first" -- used with foreach
  RecordValue a -> "{" ++ intercalate ", " ["." ++ n ++ " = " ++ prettyExp v | (n,v) <- a] ++ "}"
  NotNull e -> prettyExp e -- HACK
  Not e -> "!" ++ prettyExp e
  Map_notElem a b -> prettyExp a ++ ".count(" ++ prettyExp b ++ ")<=0"
  Map_elem a b -> prettyExp a ++ ".count(" ++ prettyExp b ++ ")>0"
  Vector_size a -> prettyExp a ++ ".size()"
  Vector_dataPtr a -> prettyExp a ++ ".data()"
  CallTypeConsructor t a -> prettyType t ++ "(" ++ prettyExp a ++ ")"
  GLConstant a -> show a
  GLCommand a -> "gl" ++ drop 2 (show a)
  x -> error $ show x
