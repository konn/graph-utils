> {-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

= Requirements
parsec >= 3.0, fgl

= INSTALL
$ cabal configure
$ cabal build
$ cabal install

= Usage

> import Data.Graph.Inductive
> import Data.Graph.EasyGrapher
> import Data.Graph.PageRank
> 
> main = do
>   print (pageRanks ([$gr| D1 -> D2, D1 -> D3, D1 -> D4, D2->D3, D2->D5, D3-> D4, D4->D1, D5 -> D3|] :: Gr String()) 0.4 0.05)
>   print ([$gr| a -> b, b -> 1, 1 -> c |] :: Gr String ())
