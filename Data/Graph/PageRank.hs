module Data.Graph.PageRank (pageRanks, RankDic(..)) where
import Control.Monad.RWS
import Data.Graph.Inductive
import Prelude hiding (map, lookup)
import Data.Map hiding (map)
import Data.Maybe (fromJust)
import Control.Monad

map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap

data Env = Env {node :: [Node], from :: Map Node [Node], outdegrees :: Map Node Int}
type RankDic = Map Node Double 
type PRMachine = RWS Env () RankDic

lookupEnv :: (Ord a) => (Env -> Map a b) -> a -> PRMachine b
lookupEnv f a = do{ dic<-asks f; return $ fromJust $ lookup a dic}

outdegree :: Node -> PRMachine Int
outdegree = lookupEnv outdegrees

froms :: Node -> PRMachine [Node]
froms = lookupEnv from

currentRank :: Node -> PRMachine Double
currentRank nd = gets (fromJust.lookup nd)

pageRanks :: (Graph gr) => gr a b -> Double -> Double -> RankDic
pageRanks gr epsilon error = fst $ execRWS steps Env{node=nds, from=froms, outdegrees=outdegs} initRanks
    where nds = nodes gr
          count :: (Num a) => a
          count = fromIntegral $ noNodes gr
          froms = fromList $ zip nds $ map (pre gr) nds
          outdegs = fromList $ zip nds $ map (outdeg gr) nds
          initRanks = fromList $ zip nds $ replicate count (1/count)
          steps = do
            old <- get
            new <- calcPageRank epsilon
            let cond = foldWithKey (\k a b -> b && ((findWithDefault (1/0) k new)-a < error)) True old
            if cond then return new else steps
            
            

calcPageRank :: Double -> PRMachine RankDic
calcPageRank epsilon = do
  nds <- asks node
  dic <- forM nds $ \n -> do
                 frms <- froms n
                 ranks <- forM frms $ \m -> do
                            deg <- outdegree m
                            rank <- currentRank m
                            return (rank/fromIntegral deg)
                 count <- liftM (fromIntegral.length) $ asks node
                 return (n, epsilon/count + (1-epsilon)*(sum ranks))
  let rdic = fromList dic
  put rdic
  return rdic