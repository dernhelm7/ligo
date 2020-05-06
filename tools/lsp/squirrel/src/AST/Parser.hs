
module AST.Parser (example, contract) where

import Data.Text (Text)

import AST.Types hiding (tuple)

import Parser
import Range

import Debug.Trace

name :: Parser (Name Range)
name = ctor Name <*> token "Name"

capitalName :: Parser (Name Range)
capitalName = ctor Name <*> token "Name_Capital"

contract :: Parser (Contract Range)
contract =
  ctor Contract
  <*> subtree "contract" do
        many "declaration" do
          inside "declaration:" do
            declaration

declaration :: Parser (Declaration Range)
declaration
  =   do ctor ValueDecl <*> binding
  <|> do ctor ValueDecl <*> vardecl
  <|> do ctor ValueDecl <*> constdecl
  <|> typedecl

typedecl :: Parser (Declaration Range)
typedecl = do
  subtree "type_decl" do
    ctor TypeDecl
      <*> inside "typeName:"  name
      <*> inside "typeValue:" newtype_

vardecl :: Parser (Binding Range)
vardecl = do
  subtree "var_decl" do
    ctor Var
      <*> inside "name:"  name
      <*> inside "type:"  type_
      <*> inside "value:" expr

constdecl :: Parser (Binding Range)
constdecl = do
  subtree "const_decl" do
    ctor Const
      <*> inside "name" name
      <*> inside "type" type_
      <*> inside "value" expr

binding :: Parser (Binding Range)
binding = do
  inside ":fun_decl" do
    ctor Function
      <*> recursive
      <*> inside "name:"  name
      <*> inside "parameters:parameters" do
            many "param" do
              notFollowedBy do
                consumeOrDie ")"

              stubbed "parameters" paramDecl
      <*> inside "type:" type_
      <*> inside "body:" letExpr

recursive = do
  mr <- optional do
    inside "recursive" do
      token "recursie"

  return $ maybe False (== "recursive") mr

expr :: Parser (Expr Range)
expr = stubbed "expr" do
  select
    [ Ident <$> getRange <*> do
        ctor QualifiedName
          <*> name
          <*> pure []
    , opCall
    , fun_call
    , record_expr
    , int_literal
    , par_call
    , method_call
    -- , if_expr
    -- , constant
    ]
  where
  -- $.case_expr,
  -- $.cond_expr,
  -- $.disj_expr,
  -- $.fun_expr,

method_call :: Parser (Expr Range)
method_call = do
  subtree "projection_call" do
    ctor Apply
      <*> do ctor Ident <*> field "f" projection
      <*> inside "arguments" arguments

projection :: Parser (QualifiedName Range)
projection = do
  subtree "data_projection" do
    ctor QualifiedName
      <*> inside "struct" name
      <*> many "selection" selection

selection :: Parser (Path Range)
selection = do
  inside "index:selection"
    $   do ctor At <*> name
    <|> do ctor Ix <*> token "Int"

par_call :: Parser (Expr Range)
par_call = do
  subtree "par_call" do
    ctor Apply
      <*> inside "f" expr
      <*> inside "arguments" arguments

int_literal :: Parser (Expr Range)
int_literal = do
  ctor Constant
    <*> do ctor Int <*> token "Int"

record_expr :: Parser (Expr Range)
record_expr = do
  subtree "record_expr" do
    ctor Record <*> do
      many "assignment" do
        inside "assignment:field_assignment" do
          ctor Assignment
            <*> inside "name" name
            <*> inside "_rhs" expr

fun_call :: Parser (Expr Range)
fun_call = do
  subtree "fun_call" do
    ctor Apply
      <*> do ctor Ident <*> inside "f" function_id
      <*> inside "arguments" arguments

arguments =
  subtree "arguments" do
    many "argument" do
      inside "argument" expr

function_id :: Parser (QualifiedName Range)
function_id = select
  [ ctor QualifiedName
      <*> name
      <*> pure []
  , do
      subtree "module_field" do
        ctor QualifiedName
          <*> inside "module" capitalName
          <*> do pure <$> do ctor At <*> inside "method" name
  ]

opCall :: Parser (Expr Range)
opCall = do
  subtree "op_expr"
    $   do inside "the" expr
    <|> do ctor BinOp
             <*> inside "arg1" expr
             <*> inside "op"   anything
             <*> inside "arg2" expr

letExpr = do
  subtree "let_expr" do
    ctor let'
      <*> optional do
        inside "locals:block" do
          many "decl" do
            inside "statement" do
              declaration <|> statement
      <*> inside "body"expr

  where
    let' r decls body = case decls of
      Just them -> Let r them body
      Nothing   -> body

statement :: Parser (Declaration Range)
statement = ctor Action <*> expr

paramDecl :: Parser (VarDecl Range)
paramDecl = do
  info <- getRange
  inside "parameter:param_decl" do
    ctor Decl
      <*> do inside ":access" do
              select
                [ ctor Mutable   <* consumeOrDie "var"
                , ctor Immutable <* consumeOrDie "const"
                ]
     <*> inside "name" name
     <*> inside "type" type_

newtype_ = select
  [ record_type
  , type_
  -- , sum_type
  ]

record_type = do
  subtree "record_type" do
    ctor TRecord
      <*> many "field" do
        inside "field" do
          field_decl

field_decl = do
  subtree "field_decl" do
    ctor TField
      <*> inside "fieldName" name
      <*> inside "fieldType" type_

type_ :: Parser (Type Range)
type_ =
    fun_type
  where
    fun_type :: Parser (Type Range)
    fun_type = do
      inside ":fun_type" do
        ctor tarrow
          <*>             inside "domain"  cartesian
          <*> optional do inside "codomain" fun_type

      where
        tarrow info domain codomain =
          case codomain of
            Just co -> TArrow info domain co
            Nothing -> domain

    cartesian = do
      inside ":cartesian" do
        ctor TProduct <*> some "corety" do
          inside "element" do
            core_type

    core_type = do
      select
        [ ctor TVar <*> name
        , subtree "invokeBinary" do
            ctor TApply
              <*> inside "typeConstr" name
              <*> inside "arguments"  typeTuple
        ]

typeTuple :: Parser [Type Range]
typeTuple = do
  subtree "type_tuple" do
    many "type tuple element" do
      inside "element" type_

example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/address.ligo"
-- example = "../../../src/test/contracts/amount.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
