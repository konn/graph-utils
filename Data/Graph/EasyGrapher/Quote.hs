{-# LANGUAGE  TemplateHaskell, NamedFieldPuns, TupleSections, DeriveDataTypeable,
              NoMonomorphismRestriction  #-}
module Data.Graph.EasyGrapher.Quote (gr, deserialize) where
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec hiding ((<|>), many, State, label)
import Control.Applicative
import Data.Graph.EasyGrapher.EasyGrapher
import Data.Generics (Data, Typeable, extR, extQ)
import Data.List

-- * Graph Parser
parseGraph :: (Monad m) => (String, Int, Int) -> String -> m (EGGraph Value)
parseGraph (file, line, col) src = do
  ans <- runParserT p () "" src
  either (fail.show) return ans
  where
    p = do
      pos <- getPosition
      setPosition $
        (flip setSourceName) file $
        (flip setSourceLine) line $
        (flip setSourceColumn) col $
        pos
      spaces *> lexeme(graphs) <* eof

data Value = Val String | Var String deriving (Typeable, Data, Ord, Eq, Show)

-- * Parser Combinators
symbol s = lexeme $ string s
lexeme p = p <* spaces
graphs = sepEndBy1 term (symbol ",")
term = try edge <|> EGVertex <$> label
edge = (:=>) <$> (label<* symbol "->") <*> label
label = (Val <$> ident) <|> try (Val <$> lit) <|> var
lit = between (symbol "\"") (symbol "\"") (many $ noneOf "\"")
  <|> show <$> between (symbol "\'") (symbol "\'") (noneOf "'")
  <|> between (symbol "(") (symbol ")") (many $ noneOf ")")
var = Var <$> (symbol "'" *> ident)
ident = lexeme $ many1 alphaNum

-- |Wrap function for read (specialized for String & Char)
deserialize :: (Data a, Read a) => String -> a
deserialize = read `extR` (id :: String -> String) `extR` chShow
  where
    chShow [x] = x
    chShow x = read x

-- | Quasi quoter for 'EGGraph'
gr :: QuasiQuoter
gr = QuasiQuoter quoteGraphExp quoteGraphPat

quoteGraphExp :: String -> ExpQ
quoteGraphExp src = do
  loc <- location
  let pos=(loc_filename loc, fst $ loc_start loc, snd $ loc_start loc)
  gr <- parseGraph pos src
  appE (varE 'buildGraph) $ dataToExpQ (const Nothing `extQ` antiStrExp) gr

antiStrExp :: Value -> Maybe ExpQ
antiStrExp (Var sym) = Just $ varE (mkName sym)
antiStrExp (Val a)   = Just $ appE (varE 'deserialize) $ litE $ stringL a

quoteGraphPat :: String -> PatQ
quoteGraphPat src = do
  loc <- location
  let pos=(loc_filename loc, fst $ loc_start loc, snd $ loc_start loc)
  gr <- parseGraph pos src
  dataToPatQ (const Nothing `extQ` antiStrPat) (sort gr)

antiStrPat :: Value -> Maybe PatQ
antiStrPat (Var sym) = Just $ varP (mkName sym)
antiStrPat (Val a) = Just $ litP $ stringL a