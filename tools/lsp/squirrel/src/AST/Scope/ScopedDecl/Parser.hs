{-# LANGUAGE RecordWildCards #-}

module AST.Scope.ScopedDecl.Parser
  ( parseType
  ) where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe, mapMaybe)
import Duplo.Tree (layer)

import AST.Pretty (PPableLIGO, ppToText)
import AST.Scope.ScopedDecl (Type (..), TypeConstructor (..), TypeField (..))
import AST.Skeleton (LIGO)
import qualified AST.Skeleton as LIGO
  (Ctor (..), FieldName (..), TField (..), Type (..), Variant (..))

-- The node is _always_ parsed as some type. In the worst case — if the node is
-- not a type, it's parsed as an alias type with the node textual representation
-- as its content.
--
-- Also see 'parseAliasType'.
parseType :: PPableLIGO info => LIGO info -> Type
parseType node =
  fromMaybe (parseAliasType node) (asum (map ($ node) parsers))
  where
    parsers =
      [ parseRecordType
      , parseVariantType
      , parseTupleType
      ]

parseRecordType :: PPableLIGO info => LIGO info -> Maybe Type
parseRecordType node = do
  LIGO.TRecord fieldNodes <- layer node
  let typeFields = mapMaybe parseTypeField fieldNodes
  pure (RecordType typeFields)

parseTypeField :: PPableLIGO info => LIGO info -> Maybe TypeField
parseTypeField node = do
  LIGO.TField nameNode typNode <- layer node
  LIGO.FieldName _tfName <- layer nameNode
  let _tfType = parseType typNode
  pure TypeField{ .. }

parseVariantType :: LIGO info -> Maybe Type
parseVariantType node = do
  LIGO.TSum conNodes <- layer node
  let cons = mapMaybe parseTypeConstructor conNodes
  pure (VariantType cons)

parseTypeConstructor :: LIGO info -> Maybe TypeConstructor
parseTypeConstructor node = do
  LIGO.Variant conNameNode _ <- layer node
  LIGO.Ctor _tcName <- layer conNameNode
  pure TypeConstructor{ .. }

parseTupleType :: PPableLIGO info => LIGO info -> Maybe Type
parseTupleType node = do
  LIGO.TProduct elementNodes <- layer node
  let elements = map parseType elementNodes
  pure (TupleType elements)

-- Since we don't care right now about distinguishing functions or whatever, we
-- just treat the whole node as a type name. It _is_ possible that the node is
-- not even a type: it could be an error node. However we choose to fail, we'll
-- lose the whole type structure instead of this one leaf.
parseAliasType :: PPableLIGO info => LIGO info -> Type
parseAliasType node = AliasType (ppToText node)
