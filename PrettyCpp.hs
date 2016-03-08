{-# LANGUAGE LambdaCase #-}
module PrettyCpp where

import Control.Monad.Writer
import Data.List
import Language hiding ((.))

pretty :: DefM () -> String
pretty defM = unlines $ map prettyDef $ execWriter defM

prettyDef :: Def -> String
prettyDef = \case
  Procedure name args retType stmts -> unlines
    [ unwords $ [prettyType retType, name, "(", intercalate ", " (map prettyArg args), ") {"]
    , unlines $ map (prettyStmt 1) stmts
    , "}"
    ]
  Method className name args retType stmts -> unlines
    [ unwords $ [prettyType retType, className ++ "::" ++ name, "(", intercalate ", " (map prettyArg args), ") {"]
    , unlines $ map (prettyStmt 1) stmts
    , "}"
    ]
  Constructor className args stmts -> unlines
    [ unwords $ [className ++ "::" ++ className, "(", intercalate ", " (map prettyArg args), ") {"]
    , unlines $ map (prettyStmt 1) stmts
    , "}"
    ]
  Destructor className stmts -> unlines
    [ unwords $ ["~" ++ className ++ "::" ++ className, "{"]
    , unlines $ map (prettyStmt 1) stmts
    , "}"
    ]

prettyArg (n :@ t) = unwords [prettyType t,n]

prettyType = \case
  Bool  -> "bool"
  Float -> "float"
  Int   -> "int32_t"
  UInt  -> "uint32_t"
  Void  -> "void"
  Class n -> n
  Enum n  -> n
  Const t -> "const " ++ prettyType t
  Ref t   -> prettyType t ++ "&"
  Ptr t   -> prettyType t ++ "*"
  SmartPtr t -> "shared_ptr<" ++ prettyType t ++ ">"

addIndentation ind s = concat (replicate ind "  ") ++ s

prettyCase ind = addIndentation ind . \case
  Case p s  -> "case " ++ prettyPat p ++ ": {\n" ++ unlines (map (prettyStmt $ ind + 1) s) ++ addIndentation (ind + 1) "break;\n" ++ addIndentation ind "}"
  Default s -> "default:\n" ++ unlines (map (prettyStmt $ ind + 1) s)

prettyPat = \case
  NsPat a -> "::" ++ intercalate "::" a

prettyStmt ind = addIndentation ind . \case
  Switch e c -> "switch(" ++ prettyExp e ++ ") {\n" ++ unlines (map (prettyCase $ ind + 1) c) ++ addIndentation ind "}\n"
  Return e -> "return " ++ prettyExp e ++ ";"
  Throw s -> "throw " ++ show s ++ ";"
  Call a b -> prettyExp a ++ "(" ++ intercalate ", " (map prettyExp b) ++ ");"
  If a b c -> "if (" ++ prettyExp a ++ ") {\n" ++ unlines (map (prettyStmt $ ind + 1) b) ++ addIndentation ind "}" ++
              if null c then "" else " else {\n" ++ unlines (map (prettyStmt $ ind + 1) c) ++ addIndentation ind "}"
  VarDef t n -> prettyType t ++ " " ++ intercalate ", " n ++ ";"
  VarADTDef t n e -> "auto " ++ n ++ " = std::static_pointer_cast<data::" ++ t ++ ">(" ++ prettyExp e ++ ");"
  VarAssignDef t n e -> prettyType t ++ " " ++ n ++ " = " ++ prettyExp e ++ ";"
  VarConstructor t n e -> prettyType t ++ " " ++ n ++ "(" ++ prettyExp e ++ ");"
  VarCharPtrFromString n e -> "const char* " ++ n ++ " = " ++ prettyExp e ++ ".c_str();"
  a := b -> prettyExp a ++ " = " ++ prettyExp b ++ ";"
  For [a] b c stmts -> "for (" ++ prettyStmt 0 a ++ prettyExp b ++ ";" ++ prettyExp c ++ ") {" ++ unlines (map (prettyStmt $ ind + 1) stmts) ++ addIndentation ind "}"
  a :/= b -> prettyExp a ++ " /= " ++ prettyExp b ++ ";"
  a :|= b -> prettyExp a ++ " |= " ++ prettyExp b ++ ";"
  Map_foreach n e s -> "for (auto " ++ n ++ " : " ++ prettyExp e ++ ") {\n" ++ unlines (map (prettyStmt $ ind + 1) s) ++ addIndentation ind "}"
  Vector_foreach n e s -> "for (auto " ++ n ++ " : " ++ prettyExp e ++ ") {\n" ++ unlines (map (prettyStmt $ ind + 1) s) ++ addIndentation ind "}"
  Vector_pushBack a b -> prettyExp a ++ ".push_back(" ++ prettyExp b ++ ");"
  Break -> "break;"
  Continue -> "continue;"
  Inc e -> prettyExp e ++ "++;"

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
  New n a -> "new " ++ n ++ "(" ++ intercalate "," (map prettyExp a) ++ ")"
  IteratorValue e -> prettyExp e ++ ".second" -- used with foreach
  IteratorKey e -> prettyExp e ++ ".first" -- used with foreach
  RecordValue a -> "{" ++ intercalate ", " ["." ++ n ++ "=" ++ prettyExp v | (n,v) <- a] ++ "}"
  NotNull e -> prettyExp e -- HACK
  Not e -> "!" ++ prettyExp e
  Map_notElem a b -> prettyExp a ++ ".count(" ++ prettyExp b ++ ")<=0"
  Map_elem a b -> prettyExp a ++ ".count(" ++ prettyExp b ++ ")>0"
