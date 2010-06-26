{-# LANGUAGE NamedFieldPuns, StandaloneDeriving, DeriveDataTypeable, TypeSynonymInstances #-}
module Data.EasyGrapher.EasyGrapher (EGGraph(..), EGTerm(..), buildGraph, fromGr) where
import Data.Graph.Inductive hiding(empty)
import qualified Data.Graph.Inductive as G
import Control.Monad
import Data.Map hiding (map, empty)
import qualified Data.Map as M
import Control.Monad.State 
import Data.Maybe
import Prelude hiding (lookup)
import Data.Generics
import qualified Data.Graph as DG
import Data.List (sort)

-- |'EGTerm' is a vertex & an edge.
data (Eq a, Ord a) => EGTerm a = a :=> a | EGVertex a deriving (Show, Eq, Typeable, Ord)
deriving instance (Data a, Ord a)=>Data (EGTerm a)

-- |'EGGraph a' is a list of 'EGTerm a'.
type EGGraph a = [EGTerm a]

data Env gr a = Env{graph :: gr a (), dic :: Map a Node}
empty :: (Eq a, DynGraph gr) => Env gr a
empty = Env{graph = G.empty, dic = M.empty}

type GrMachine gr lab a = State (Env gr lab) a

-- |'buildGraph' converts EGGraph 'gr' into the '(gr a ())'
buildGraph :: (DynGraph gr, Ord a) => EGGraph a -> gr a ()
buildGraph gr = evalState (build gr) empty

build :: (Ord lab, DynGraph gr) => EGGraph lab -> GrMachine gr lab (gr lab ())
build [] = gets graph 
build ((lab1 :=> lab2):xs) = do
  [n1, n2] <- mapM toNode [lab1, lab2]
  env@Env{graph} <- get
  put $ env{graph=insEdge (n1, n2, ()) graph}
  build xs
build ((EGVertex lab):xs) = toNode lab >> build xs

toNode :: (Ord lab, DynGraph gr) => lab -> GrMachine gr lab Node
toNode lab = do
    cond <- gets $ notMember lab . dic
    when cond $ mkNode lab
    gets $ fromJust . lookup lab . dic
  where
    mkNode :: (Ord lab, DynGraph gr) => lab -> GrMachine gr lab ()
    mkNode lab = do
      (nd:_) <- gets (newNodes 1 . graph)
      env@Env{graph, dic} <- get
      put $ env{graph=insNode (nd, lab) graph, dic=insert lab nd dic}

-- |'fromGr' converts 'gr :: (gr a ())' into 'EGGraph a'
fromGr :: (Graph gr, Ord a) => gr a () -> EGGraph a
fromGr gr = sort $ map (uncurry (:=>).(\(a,b)->(toL a, toL b))) $ edges gr
  where toL = fromJust . lab gr 
