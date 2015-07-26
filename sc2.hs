{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Arrow ((&&&))
import           Control.Lens (over)
import           Control.Lens.TH (makeLenses)
import           Data.Bool (bool)
import           Data.Function ((&), on)
import           Data.List (sortBy)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import           Data.Set (Set)
import qualified Data.Set as Set

data Unit = SCV | Marine deriving (Show, Eq, Ord)
data Building = SupplyDepot | Barracks | CommandCenter deriving (Show, Eq, Ord)

data Attributes = Attributes { time :: Int
                             , cost :: (Int, Int) -- (minerals, gas)
                             , supply :: Int
                             }

data State = State { _units :: Map Unit Int
                   , _buildings :: Map Building Int
                   } deriving (Show)
makeLenses ''State

data Goal = GoalUnits Unit Int

type BuildOrder = [Order]
data Order = Train Unit
           | Build Building
           deriving (Show, Ord, Eq)

attributeDB :: Map (Either Unit Building) Attributes
attributeDB = Map.fromList
  [ (Left SCV,            Attributes { time = 12, cost = ( 50,  0), supply = -1})
  , (Left Marine,         Attributes { time = 18, cost = ( 50,  0), supply = -1})
  , (Right SupplyDepot,   Attributes { time = 21, cost = (100,  0), supply =  8})
  , (Right Barracks,      Attributes { time = 46, cost = (150,  0), supply =  0})
  , (Right CommandCenter, Attributes { time = 71, cost = (400,  0), supply = 15})
  ]

dependencyDB :: Map (Either Unit Building) (Maybe Building)
dependencyDB = Map.fromList
  [ (Left SCV, Just CommandCenter)
  , (Left Marine, Just Barracks)
  , (Right SupplyDepot, Nothing)
  , (Right Barracks, Nothing)
  , (Right CommandCenter, Nothing)
  ]

getDependency :: Either Unit Building -> Maybe Building
getDependency u = fromMaybe err (Map.lookup u dependencyDB)
    where
        err = error $ "dependencyDB is missing an entry for " ++ (show u) -- It would be nice if this was checked at compile time (dependent types ?)

rawData :: [(Int, Int)] -- scvs. minerals per minute
rawData = zip [12..] $ map (uncurry (-))
  [ (710, 50)
  , (1905, 1145)
  , (2840, 2015)
  , (3850, 2990)
  , (5660, 4730)
  , (7190, 6330)
  ]

startingState :: State
startingState = State { _units = Map.fromList [(SCV, 12)]
                      , _buildings = Map.fromList [(CommandCenter, 1)]
                      }
                     

go :: Goal -> Maybe BuildOrder
go = fmap fst . listToMaybe .
     sortBy (compare `on` snd) . map (id &&& eval) . Set.toList .
     go' Set.empty [] startingState

lookupInt :: (Ord k) => k -> Map k Int -> Int
lookupInt k = fromMaybe 0 . Map.lookup k

updateInt :: (Ord k) => (Int -> Int) -> k -> Map k Int -> Map k Int
updateInt f k m = Map.insert k (f $ fromMaybe 0 $ Map.lookup k m) m

units' :: Unit -> State -> Int
units' u = lookupInt u . _units

buildings' :: Building -> State -> Int
buildings' b = lookupInt b . _buildings

scvsPerBase :: Int
scvsPerBase = 16

productionPrune :: Int -> Int -> Bool
productionPrune prods goalUnits = prods * prods > goalUnits

go' :: Set BuildOrder -> BuildOrder -> State -> Goal -> Set BuildOrder
go' accs acc s g@(GoalUnits u n)
  | units' u s >= n = Set.insert acc accs
  | otherwise = catMaybes 
      [ bool Nothing (Just $ Train SCV) (units' SCV s < scvsPerBase)
      , Just $ getDependency (Left u) & 
          maybe (Train u) 
                (\d -> buildings' d s > 0 &
                       bool (Build d) (Train u))
      , (\d -> bool (Just $ Build d) Nothing $ productionPrune (buildings' d s) n) =<< getDependency (Left u)
      ] & Set.unions . map (\o -> go' accs (acc ++ [o]) (step o s) g)

step :: Order -> State -> State
step (Build b) s = over buildings (updateInt (+1) b) s
step (Train u) s = over units (updateInt (+1) u) s

eval :: BuildOrder -> Int
eval = undefined

main :: IO ()
main = mapM_ print $ go' Set.empty [] startingState (GoalUnits Marine 1)
