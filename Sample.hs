{-# LANGUAGE QuasiQuotes, TemplateHaskell, ViewPatterns #-}
-- modules of THIS package
import Data.Graph.EasyGrapher
import Data.Graph.PageRank
-- module of fgl
import Data.Graph.Inductive

-- Standard EGGraph Notation & buildGraph
egg1 :: EGGraph String
egg1 = [ 
  "D1" :=> "D2", "D1" :=> "D3", "D1" :=> "D4", "D2" :=> "D3", "D2" :=> "D5",
  "D3" :=> "D4", "D4" :=> "D1", "D5" :=> "D3"
 ]
graph1 :: Gr String ()
graph1 = buildGraph egg1 -- use function "buildGraph" to Build Graph.

-- Same Graph by Quasi Quotes
-- Quasi Quotes supports only (Gr String ()) for time being.
graph1' :: Gr String ()
graph1' = [$gr| D1 -> D2, D1 -> D3, D1 -> D4, D2 -> D3, D2 -> D5,
                D3->D4, D4->D1, D5->D3 |]

-- Antiquote pattern
-- Node starting with ' is antiquote. For example, "'s" means 
-- the value of local variable s.
mkCyclicGraphWith :: String -> Gr String ()
mkCyclicGraphWith s = [$gr| 's -> 1, 1 -> a, a -> 3c, 3c -> 's |] 

-- Pattern Match
-- QuasiQuote is defiend for EGGraph, so we have to convert (gr a ())
-- into (EGGraph a) using fromGr. Use "View pattern" (ghc extension)
-- to use pattern match with type (gr a ()).
leftOfSingleton :: (Ord a) => (Gr a ()) -> Maybe a
leftOfSingleton (fromGr -> [$gr| 'x -> 'y |]) = Just x
leftOfSingleton _ = Nothing

-- |Version 0.3.7 features

-- Polymorphism Support (Only for Expression Quasi Quotations)
-- You can create (Read a, Data a) => (Gr a ()) using Quasi Quotations!
pgr1 :: Gr Int ()
pgr1 = [$gr| 12 -> 23, 23 -> 34, 4, 23 -> 56, 56 -> 12 |]

-- Supported literal notation (for String & Char at this time)
pgr2 :: Gr Char ()
pgr2 = [$gr| 'a' -> 'b', 'c' -> 'd', 'f' |]
pgr3 :: Gr String ()
pgr3 = [$gr| "hoge" -> "bar", "foo bar baz" -> "bar" |]
-- You can omit ' or " if the value of literal doesn't contain whitespace.
pgr2' :: Gr Char ()
pgr2' = [$gr| a -> b, c -> 'd', f |]
pgr3' :: Gr String ()
pgr3' = [$gr| hoge -> bar, "foo bar baz" -> bar |]

-- Group notation
pgr4 :: Gr (Maybe Int) ()
pgr4 = [$gr| (Just 3) -> Nothing, (Just 2) -> (Just 3) |] 

main = do
  putStr "This is the graph converted from standard EGGraph form."
  print graph1
  putStrLn "\nThis is converted from following EGGraph: "
  print egg1
  putStr "\nAnd this is the same graph converted from Quasi Quotes:"
  print graph1'
  putStr "\n\nAntiquote.\nmkCyclicGraphWith \"foobar\" = "
  print $ mkCyclicGraphWith "foobar"
  putStrLn "\n\nWe can calculate Pageranks."
  putStr "pageRanks graph1 0.4 0.05 = "
  print $ pageRanks graph1 0.4 0.05
  putStr "\n\nPattern match.\n leftOfSingleton [$gr| 1 |] = "
  print (leftOfSingleton [$gr| 1 |] :: Maybe String)
  putStr " leftOfSingleton [$gr| 12 -> 23 |] = "
  print (leftOfSingleton [$gr| 12 -> 23 |] :: Maybe String)
  putStr " leftOfSingleton [$gr| 12 -> 23, 23 -> 34 |] = "
  print (leftOfSingleton [$gr| 12 ->23, 23 ->34 |] :: Maybe String)
