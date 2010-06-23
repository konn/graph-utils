{-# LANGUAGE TemplateHaskell, NamedFieldPuns, TupleSections  #-}
module Quote (gr) where
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec hiding ((<|>), many, State)
import Control.Applicative
import EasyGrapher

parseGraph :: (Monad m) => (String, Int, Int) -> String -> m (EGGraph String)
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

symbol s = lexeme $ string s
lexeme :: Parsec String u a -> Parsec String u a
lexeme p = p <* spaces
graphs :: Parsec String () (EGGraph String)
graphs = try(sepBy1 edge (symbol ","))
edge = (:=>) <$> (ident <* symbol "->") <*> ident
ident = lexeme $ many1 alphaNum

gr :: QuasiQuoter
gr = QuasiQuoter quoteGraphExp quoteGraphPat

quoteGraphExp :: String -> ExpQ
quoteGraphExp src = do
  loc <- location
  let pos=(loc_filename loc, fst $ loc_start loc, snd $ loc_start loc)
  gr <- parseGraph pos src
  appE (varE 'buildGraph) $ dataToExpQ (const Nothing) gr

quoteGraphPat :: String -> PatQ
quoteGraphPat src = do
  loc <- location
  let pos=(loc_filename loc, fst $ loc_start loc, snd $ loc_start loc)
  gr <- parseGraph pos src
  dataToPatQ (const Nothing) gr