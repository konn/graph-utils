{-# LANGUAGE  TemplateHaskell, NamedFieldPuns, TupleSections, DeriveDataTypeable,
              NoMonomorphismRestriction  #-}
module Data.Graph.EasyGrapher.Quote (gr) where
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec hiding ((<|>), many, State, label)
import Control.Applicative
import Data.Graph.EasyGrapher.EasyGrapher
import Data.Generics
import Data.List

-- * Graph Parser
parseGraph :: (Monad m) => (String, Int, Int) -> String -> m (EGGraph Value)
parseGraph (file, line, col) src = 
  case (parse p "" src) of
    Left err -> fail $ show err
    Right gr -> return gr
  where
    p = do
      pos <- getPosition
      setPosition $
        (flip setSourceName) file $
        (flip setSourceLine) line $
        (flip setSourceColumn) col $
        pos
      spaces *> lexeme(graphs)

data Value = Val String | Var String deriving (Typeable, Data, Ord, Eq, Show)

-- * Parser Combinators
symbol s = lexeme $ string s
lexeme p = p <* spaces
graphs = sepEndBy1 term (symbol ",")
term = try edge <|> EGVertex <$> label
edge = (:=>) <$> (label<* symbol "->") <*> label
label = var <|> (Val <$> deserialize <$> ident)
var = Var <$> (symbol "'" *> ident)
ident = lexeme $ many1 alphaNum

deserialize :: (Data a, Typeable a, Read a) => String -> a
deserialize = read `extR` (id :: String -> String) 


-- | Quasi quoter for EGGraph
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
antiStrExp (Val a) = Just $ litE $ stringL a
antiStrExp _ = Nothing

quoteGraphPat :: String -> PatQ
quoteGraphPat src = do
  loc <- location
  let pos=(loc_filename loc, fst $ loc_start loc, snd $ loc_start loc)
  gr <- parseGraph pos src
  dataToPatQ (const Nothing `extQ` antiStrPat) (sort gr)

antiStrPat :: Value -> Maybe PatQ
antiStrPat (Var sym) = Just $ varP (mkName sym)
antiStrPat (Val a) = Just $ litP $ stringL a
antiStrPat _ = Nothing