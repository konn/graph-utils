{-# LANGUAGE NamedFieldPuns, StandaloneDeriving, DeriveDataTypeable #-}
module Data.Graph.EasyGrapher (EGGraph(..), EGEdge(..), buildGraph) where
import Data.Graph.Inductive hiding(empty)
import qualified Data.Graph.Inductive as G
import Control.Monad
import Data.Map hiding (map, empty)
import qualified Data.Map as M
import Control.Monad.State 
import Data.Maybe
import Prelude hiding (lookup)
import Data.Generics


data (Eq a, Ord a) => EGEdge a = a :=> a deriving (Show, Eq, Typeable)
deriving instance (Data a, Ord a)=>Data (EGEdge a)
type EGGraph a = [EGEdge a]

data Env gr a = Env{graph :: gr a (), dic :: Map a Node}
empty :: (Eq a, DynGraph gr) => Env gr a
empty = Env{graph = G.empty, dic = M.empty}

type GrMachine gr lab a = State (Env gr lab) a

buildGraph :: (DynGraph gr, Ord a) => EGGraph a -> gr a ()
buildGraph xs = evalState (build xs) empty

build :: (Ord lab, DynGraph gr) => [EGEdge lab] -> GrMachine gr lab (gr lab ())
build [] = gets graph 
build ((lab1 :=> lab2):xs) = do
    [n1, n2] <- mapM toNode [lab1, lab2]
    env@Env{graph} <- get
    put $ env{graph=insEdge (n1, n2, ()) graph}
    build xs
  where
    toNode :: (Ord lab, DynGraph gr) => lab -> GrMachine gr lab Node
    toNode lab = do
      cond <- gets $ notMember lab . dic
      when cond $ mkNode lab
      gets $ fromJust . lookup lab . dic
    mkNode :: (Ord lab, DynGraph gr) => lab -> GrMachine gr lab ()
    mkNode lab = do
      (nd:_) <- gets (newNodes 1 . graph)
      env@Env{graph, dic} <- get
      put $ env{graph=insNode (nd, lab) graph, dic=insert lab nd dic}
