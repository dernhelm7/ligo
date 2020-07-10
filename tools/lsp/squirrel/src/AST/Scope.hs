
{-# language Strict #-}

{- | /The/ scope resolution system.
-}

module AST.Scope
  -- ( HasLocalScope (..)
  -- , addLocalScopes
  -- , lookupEnv
  -- , Kind (..)
  -- , ScopedDecl (..)
  -- )
  where

import           Control.Arrow (second)
import           Control.Monad.State

import qualified Data.List   as List
import           Data.Map            (Map)
import qualified Data.Map    as Map
import           Data.Maybe          (listToMaybe)
import           Data.Sum            (Element, Apply, Sum)
import           Data.Text           (Text)

-- import           AST.Parser
import           AST.Types
-- import           Comment
import           Lattice
-- import           Parser
import           Pretty
import           Product
import           Range
import           Tree

-- import           Debug.Trace

type CollectM = State (Product [FullEnv, [Range]])

type FullEnv = Product ["vars" := Env, "types" := Env]
type Env     = Map Range [ScopedDecl]

data Category = Variable | Type

-- | The type/value declaration.
data ScopedDecl = ScopedDecl
  { _sdName    :: Pascal ()
  , _sdOrigin  :: Range
  , _sdBody    :: Maybe Range
  , _sdType    :: Maybe (Either (Pascal ()) Kind)
  , _sdRefs    :: [Range]
  }
  deriving Show via PP ScopedDecl

-- | The kind.
data Kind = Star
  deriving Show via PP Kind

emptyEnv :: FullEnv
emptyEnv
  = Cons (Tag Map.empty)
  $ Cons (Tag Map.empty)
    Nil

with :: Category -> FullEnv -> (Env -> Env) -> FullEnv
with Variable env f = modTag @"vars"  f env
with Type     env f = modTag @"types" f env

ofCategory :: Category -> ScopedDecl -> Bool
ofCategory Variable ScopedDecl { _sdType = Just (Right Star) } = False
ofCategory Variable _                                          = True
ofCategory Type     ScopedDecl { _sdType = Just (Right Star) } = True
ofCategory _        _                                          = False

-- | Calculate scopes and attach to all tree points declarations that are
--   visible there.
--
addLocalScopes
  :: Contains Range xs
  => Pascal (Product xs)
  -> Pascal (Product ([ScopedDecl] : Maybe Category : xs))
addLocalScopes tree =
    fmap (\xs -> Cons (fullEnvAt envWithREfs (getRange xs)) xs) tree1
  where
    tree1       = addNameCategories tree
    envWithREfs = getEnvTree tree

addNameCategories
  :: Contains Range xs
  => Pascal (Product xs)
  -> Pascal (Product (Maybe Category : xs))
addNameCategories tree = flip evalState emptyEnv do
  traverseMany
    [ Visit \r (Name t) -> do
        modify $ getRange r `addRef` (Variable, t)
        return $ (Cons (Just Variable) r, Name t)

    , Visit \r (TypeName t) -> do
        modify $ getRange r `addRef` (Type, t)
        return $ (Cons (Just Type) r, TypeName t)
    ]
    (Cons Nothing)
    tree

getEnvTree
  :: ( UpdateOver CollectM (Sum fs) (Tree fs b)
     , Apply Foldable fs
     , Apply Functor fs
     , Apply Traversable fs
     , HasRange b
     , Element Name fs
     , Element TypeName fs
     )
  => Tree fs b
  -> FullEnv
getEnvTree tree = envWithREfs
  where
    envWithREfs = flip execState env do
      traverseMany
        [ Visit \r (Name t) -> do
            modify $ getRange r `addRef` (Variable, t)
            return $ (r, Name t)

        , Visit \r (TypeName t) -> do
            modify $ getRange r `addRef` (Type, t)
            return $ (r, TypeName t)
        ]
        id
        tree

    env
      = execCollectM
      $ traverseTree pure tree

fullEnvAt :: FullEnv -> Range -> [ScopedDecl]
fullEnvAt fe r = envAt (getTag @"types" fe) r <> envAt (getTag @"vars" fe) r

envAt :: Env -> Range -> [ScopedDecl]
envAt env pos =
    Map.elems scopes
  where
    ranges = List.sortBy partOrder $ filter isCovering $ Map.keys env
    scopes = Map.unions $ (map.foldMap) toScopeMap $ map (env Map.!) ranges

    isCovering = (pos <?)
    toScopeMap sd@ScopedDecl {_sdName} = Map.singleton (ppToText _sdName) sd

addRef :: Range -> (Category, Text) -> FullEnv -> FullEnv
addRef r (categ, n) env =
  with categ env \slice ->
    Map.union
      (go slice $ range slice)
      slice
  where
    go slice (r' : rest) =
      let decls = slice Map.! r'
      in
        case updateOnly n r addRefToDecl decls of
          (True,  decls') -> Map.singleton r' decls'
          (False, decls') -> Map.insert    r' decls' (go slice rest)
    go _ [] = Map.empty

    range slice
      = List.sortBy partOrder
      $ filter (r <?)
      $ Map.keys slice

    addRefToDecl sd = sd
      { _sdRefs = r : _sdRefs sd
      }

updateOnly
  :: Text
  -> Range
  -> (ScopedDecl -> ScopedDecl)
  -> [ScopedDecl]
  -> (Bool, [ScopedDecl])
updateOnly name r f = go
  where
    go = \case
      d : ds
        | ppToText (_sdName d) == name ->
          if r == _sdOrigin d
          then         (True,   d : ds)
          else         (True, f d : ds)
        | otherwise -> second (d :) (go ds)

      [] -> (False, [])

enter :: Range -> CollectM ()
enter r = do
  modify $ modElem (r :)

define :: Category -> ScopedDecl -> CollectM ()
define categ sd = do
  r <- gets (head . getElem @[Range])
  modify
    $ modElem @FullEnv \env ->
        with categ env
        $ Map.insertWith (++) r [sd]

leave :: CollectM ()
leave = modify $ modElem @[Range] tail

-- | Run the computation with scope starting from empty scope.
execCollectM :: CollectM a -> FullEnv
execCollectM action = getElem $ execState action $ Cons emptyEnv (Cons [] Nil)

instance {-# OVERLAPS #-} Pretty FullEnv where
  pp = block . map aux . Map.toList . mergeFE
    where
      aux (r, fe) =
        pp r `indent` block fe

      mergeFE fe = getTag @"vars" @Env fe <> getTag @"types" fe

instance Pretty ScopedDecl where
  pp (ScopedDecl n o _ t refs) = color 3 (pp n) <+> pp o <+> ":" <+> color 4 (maybe "?" (either pp pp) t) <+> "=" <+> pp refs

instance Pretty Kind where
  pp _ = "TYPE"

-- | Search for a name inside a local scope.
lookupEnv :: Text -> [ScopedDecl] -> Maybe ScopedDecl
lookupEnv name = listToMaybe . filter ((name ==) . ppToText . _sdName)

-- | Add a type declaration to the current scope.
defType :: HasRange a => Pascal a -> Kind -> Pascal a -> CollectM ()
defType name kind body = do
  define Type
    $ ScopedDecl
      (void name)
      (getRange $ infoOf name)
      (Just $ getRange $ infoOf body)
      (Just (Right kind))
      []

-- observe :: Pretty i => Pretty res => Text -> i -> res -> res
-- observe msg i res
--   = traceShow (pp msg, "INPUT", pp i)
--   $ traceShow (pp msg, "OUTPUT", pp res)
--   $ res

-- | Add a value declaration to the current scope.
def
  :: HasRange a
  => Pascal a
  -> Maybe (Pascal a)
  -> Maybe (Pascal a)
  -> CollectM ()
def name ty body = do
  define Variable
    $ ScopedDecl
      (void name)
      (getRange $ infoOf name)
      ((getRange . infoOf) <$> body)
      ((Left . void) <$> ty)
      []

instance UpdateOver CollectM Contract (Pascal a) where
  before r _ = enter r
  after  _ _ = skip

instance HasRange a => UpdateOver CollectM Declaration (Pascal a) where
  before _ = \case
    TypeDecl ty body -> defType ty Star body
    _ -> skip

instance HasRange a => UpdateOver CollectM Binding (Pascal a) where
  before r = \case
    Function recur name _args ty body -> do
      when recur do
        def name (Just ty) (Just body)
      enter r

    _ -> enter r

  after _ = \case
    Irrefutable name    body -> do leave; def name  Nothing  (Just body)
    Var         name ty body -> do leave; def name (Just ty) (Just body)
    Const       name ty body -> do leave; def name (Just ty) (Just body)
    Function recur name _args ty body -> do
      leave
      unless recur do
        def name (Just ty) (Just body)

instance HasRange a => UpdateOver CollectM VarDecl (Pascal a) where
  after _ (Decl _ name ty) = def name (Just ty) Nothing

instance UpdateOver CollectM Mutable (Pascal a)
instance UpdateOver CollectM Type    (Pascal a)
instance UpdateOver CollectM Variant (Pascal a)
instance UpdateOver CollectM TField  (Pascal a)

instance HasRange a => UpdateOver CollectM Expr (Pascal a) where
  before r = \case
    Let    {} -> enter r
    Lambda {} -> enter r
    ForLoop k _ _ _ -> do
      enter r
      def k Nothing Nothing

    ForBox k mv _ _ _ -> do
      enter r
      def k Nothing Nothing
      maybe skip (\v -> def v Nothing Nothing) mv

    _ -> skip

  after _ = \case
    Let     {} -> leave
    Lambda  {} -> leave
    ForLoop {} -> leave
    ForBox  {} -> leave
    _ -> skip

instance HasRange a => UpdateOver CollectM Alt (Pascal a) where
  before r _ = enter r
  after  _ _ = leave

instance UpdateOver CollectM LHS             (Pascal a)
instance UpdateOver CollectM MapBinding      (Pascal a)
instance UpdateOver CollectM Assignment      (Pascal a)
instance UpdateOver CollectM FieldAssignment (Pascal a)
instance UpdateOver CollectM Constant        (Pascal a)

instance HasRange a => UpdateOver CollectM Pattern (Pascal a) where
  before _ = \case
    IsVar n -> def n Nothing Nothing
    _       -> skip

instance UpdateOver CollectM QualifiedName (Pascal a)
instance UpdateOver CollectM Path          (Pascal a)
instance UpdateOver CollectM Name          (Pascal a)
instance UpdateOver CollectM TypeName      (Pascal a)
instance UpdateOver CollectM FieldName     (Pascal a)
