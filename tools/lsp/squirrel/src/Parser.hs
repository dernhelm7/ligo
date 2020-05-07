
module Parser (module Parser, gets, pfGrove) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

import Data.Foldable
import Data.Text.Encoding
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text

import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)

import ParseTree
import Range
import Pretty

import Debug.Trace

-- | Parse error.
data Error
  = Expected
    { eMsg   :: Text   -- ^ Description of what was expected.
    , eWhole :: Text   -- ^ Offending text.
    , eRange :: Range  -- ^ Location of the error.
    }
  deriving (Show) via PP Error

instance Pretty Error where
  pp (Expected msg found r) = "░" <> pp msg <> pp r <> "▒" <> pp found <> "▓"

-- | Parser of tree-sitter-made tree.
newtype Parser a = Parser
  { unParser
      :: WriterT [Error]      -- Early I though to report errors that way. Obs.
      (  ReaderT ParserEnv    -- Range/Something.
      (  StateT  ParseForest  -- Current forest to recognise.
      (  ExceptT Error        -- Backtracking. Change `Error` to `()`?
      (  IO ))))              -- I forgot why. `#include`? Debug via `print`?
         a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState   ParseForest
    , MonadWriter [Error]
    , MonadReader  ParserEnv
    , MonadError   Error
    , MonadIO
    )

-- | Generate error originating at current location.
makeError :: Text -> Parser Error
makeError msg = do
  rng <- getRange
  makeError' msg rng

-- | Generate error originating at given location.
makeError' :: Text -> Range -> Parser Error
makeError' msg rng = do
  rng <- getRange
  src <- cutOut rng
  return Expected
    { eMsg   = msg
    , eWhole = src
    , eRange = rng
    }

-- | Pick next tree in a forest or die with msg.
takeNext :: Text -> Parser ParseTree
takeNext msg = do
  st@Forest {pfGrove, pfRange} <- get
  case pfGrove of
    [] -> die msg
    (_, t) : f -> do
      put st
        { pfRange = diffRange pfRange (ptRange t)
        , pfGrove = f
        }
      return t

-- | Pick a tree with that /field name/ or die with name as msg.
--
--   Will erase all subtrees with different names on the path!
--
field :: Text -> Parser a -> Parser a
field name parser = do
  grove <- gets pfGrove
  case grove of
    (name', t) : _
      | name == name' -> do
        sandbox True t

    _ -> do
      case lookup name grove of
        Just tree -> sandbox False tree
        Nothing   -> die name

  where
    sandbox firstOne tree@ParseTree {ptID, ptRange} = do
      st@Forest {pfGrove = grove, pfRange = rng} <- get
      let grove' = delete name grove
      put Forest
        { pfID    = ptID
        , pfGrove = [(name, tree)]
        , pfRange = ptRange
        }

      parser <* put st
        { pfGrove = grove'
        , pfRange = if firstOne then diffRange rng ptRange else rng
        }

-- | Variuos error reports.
fallback  :: Stubbed a => Text          -> Parser a
fallback' :: Stubbed a => Text -> Range -> Parser a
die       ::              Text          -> Parser a
die'      ::              Text -> Range -> Parser a
complain  ::              Text -> Range -> Parser ()
fallback  msg     = pure . stub =<< makeError  msg
fallback' msg rng = pure . stub =<< makeError' msg rng
die       msg     = throwError  =<< makeError  msg
die'      msg rng = throwError  =<< makeError' msg rng
complain  msg rng = tell . pure =<< makeError' msg rng

-- | If a parser fails, return stub with error originating here.
stubbed :: Stubbed a => Text -> Parser a -> Parser a
stubbed msg parser = do
  parser <|> fallback msg

-- | The forest must start with tree of that name. Its subtrees become new
--   forest. Otherwise, it dies with name as msg.
subtree :: Text -> Parser a -> Parser a
subtree msg parser = do
  ParseTree {ptChildren, ptName} <- takeNext msg
  if ptName == msg
  then do
    save <- get
    put ptChildren
    parser <* put save
  else do
    die msg

-- | Because `ExceptT` requires error to be `Monoid` for `Alternative`.
(<|>) :: Parser a -> Parser a -> Parser a
Parser l <|> Parser r = Parser (l `catchError` const r)

select :: [Parser a] -> Parser a
select = foldl1 (<|>)

optional :: Parser a -> Parser (Maybe a)
optional p = fmap Just p <|> return Nothing

-- | Custom `Alternative.many`.
--
--   TODO: remove msg.
--
many :: Text -> Parser a -> Parser [a]
many msg p = many'
  where
    many' = some' <|> pure []
    some' = do
      hasPossibleInput
      (x, consumed) <- productive p
      if consumed then do
        xs <- many'
        return (x : xs)
      else do
        return [x]

-- | Custom `Alternative.some`.
--
--   TODO: remove msg.
--
some :: Text -> Parser a -> Parser [a]
some msg p = some'
  where
    many' = some' <|> pure []
    some' = do
      hasPossibleInput
      (x, consumed) <- productive p
      if consumed then do
        xs <- many'
        return (x : xs)
      else do
        return [x]

-- | Get UID of current tree. Obsolete.
--
--   TODO: remove.
--
getTreeID :: Parser (Maybe Int)
getTreeID = Parser do
  pfGrove <$> get >>= return . \case
    [] -> Nothing
    (_, tree) : _ -> Just (ptID tree)

-- | Assert the parser consumes input. Obsolete.
--
--   TODO: remove.
--
productive :: Parser a -> Parser (a, Bool)
productive p = do
  was <- getTreeID
  res <- p
  now <- getTreeID
  return (res, was /= now)

-- | The `not <$> eos`. Obsolete.
--
--   TODO: remove.
--
hasPossibleInput :: Parser ()
hasPossibleInput = do
  yes <- gets (not . null . pfGrove)
  unless yes do
    die "something"

-- | The source of file being parsed. BS, because tree-sitter has offsets
--   in /bytes/.
data ParserEnv = ParserEnv
  { peSource :: ByteString
  }

-- | Debug print via IO. Obsolete.
--
--   TODO: remove. Also, remove IO from Parser tf stack.
--
puts :: MonadIO m => Show a => a -> m ()
puts = liftIO . print

-- | Run parser on given file.
--
--   TODO: invent /proper/ 'ERROR'-node collector.
--
runParser :: Parser a -> FilePath -> IO (a, [Error])
runParser (Parser parser) fin = do
  pforest <- toParseTree fin
  text    <- ByteString.readFile fin
  res <-
             runExceptT
      $ flip runStateT pforest
      $ flip runReaderT (ParserEnv text)
      $      runWriterT
      $ parser

  either (error . show) (return . fst) res

-- | Run parser on given file and pretty-print stuff.
debugParser :: Parser a -> FilePath -> IO a
debugParser parser fin = do
  (res, errs) <- runParser parser fin
  putStrLn "Errors:"
  for_ errs (print . nest 2 . pp)
  putStrLn ""
  putStrLn "Result:"
  return res

-- | Consume next tree if it has give name. Or die.
token :: Text -> Parser Text
token node = do
  tree@ParseTree {ptName, ptRange} <- takeNext node
  if ptName == node
  then do
    cutOut ptRange

  else do
    die' node ptRange

-- | Consume next tree, return its textual representation.
anything :: Parser Text
anything = do
  tree <- takeNext "anything"
  cutOut $ ptRange tree

-- | TODO: remove, b/c obsolete.
consume :: Text -> Parser ()
consume node = do
  ParseTree {ptName, ptRange} <- takeNext node
  when (ptName /= node) do
    complain node ptRange

-- | TODO: remove, its literally is `void . token`.
consumeOrDie :: Text -> Parser ()
consumeOrDie node = do
  ParseTree {ptName, ptRange} <- takeNext node
  when (ptName /= node) do
    die' node ptRange

-- | Extract textual representation of given range.
cutOut :: Range -> Parser Text
cutOut (Range (_, _, s) (_, _, f)) = do
  bs <- asks peSource
  return $ decodeUtf8 $ ByteString.take (f - s) (ByteString.drop s bs)

-- | Get range of current tree or forest before the parser was run.
range :: Parser a -> Parser (a, Range)
range parser =
  get >>= \case
    Forest {pfGrove = (,) _ ParseTree {ptRange} : _} -> do
      a <- parser
      return (a, ptRange)

    Forest {pfRange} -> do
      a <- parser
      return (a, pfRange)

-- | Get current range.
getRange :: Parser Range
getRange = snd <$> range (return ())

-- | Remove all keys until given key is found; remove the latter as well.
--
--   Notice: this works differently from `Prelude.remove`!
--
delete :: Eq k => k -> [(k, v)] -> [(k, v)]
delete _ [] = []
delete k ((k', v) : rest) =
  if k == k'
  then rest
  else delete k rest

-- | Parser negation.
notFollowedBy :: Parser a -> Parser ()
notFollowedBy parser = do
  good <- do
      parser
      return False
    <|> do
      return True

  unless good do
    die "notFollowedBy"

-- | For types that have a default replacer with an `Error`.
class Stubbed a where
  stub :: Error -> a

instance Stubbed Text where
  stub = pack . show

-- | This is bad, but I had to.
--
--   TODO: find a way to remove this instance.
--
instance Stubbed [a] where
  stub _ = []

-- | `Nothing` would be bad default replacer.
instance Stubbed a => Stubbed (Maybe a) where
  stub = Just . stub

-- | Universal accessor.
--
--   Usage:
--
--   > inside "$field:$treename"
--   > inside "$field"
--   > inside ":$treename" -- don't, use "subtree"
--
inside :: Stubbed a => Text -> Parser a -> Parser a
inside sig parser = do
  let (f, st') = Text.breakOn ":" sig
  let st       = Text.drop 1 st'
  if Text.null f
  then do
    -- The order is important.
    subtree st do
      stubbed f do
        parser
  else do
    field f do
      stubbed f do
        if Text.null st
        then do
          parser
        else do
          subtree st do
            parser

-- Auto-accumulated information to be fed into AST being build.
data ASTInfo = ASTInfo
  { aiRange    :: Range
  , aiComments :: [Text]
  }

-- | Equip given constructor with info.
ctor :: (ASTInfo -> a) -> Parser a
ctor = (<$> (ASTInfo <$> getRange <*> pure []))

-- | /Actual/ debug pring.
dump :: Parser ()
dump = gets pfGrove >>= traceShowM
