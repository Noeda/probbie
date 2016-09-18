{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Numeric.Probability.Monad.Toys
  ( CoinModel()
  , lowCoinModel
  , fairCoinModel
  , unfairCoinModel
  , highCoinModel
  , coinModel
  , coinModelProducesData
  , generateData
  , optimize )
  where

import Control.Monad.Primitive
import Data.Data
import Data.Traversable
import GHC.Generics
import Numeric.Probability.Monad
import System.Random.MWC
import qualified Data.Vector as V

data CoinModel f = CoinModel
  { coins :: !(V.Vector f) }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Functor, Foldable, Traversable )

lowCoinModel :: Fractional f => CoinModel f
lowCoinModel = CoinModel { coins = V.fromList [0.1, 0.1, 0.1, 0.1] }

highCoinModel :: Fractional f => CoinModel f
highCoinModel = CoinModel { coins = V.fromList [0.9, 0.9, 0.9, 0.9] }

fairCoinModel :: Fractional f => CoinModel f
fairCoinModel = CoinModel { coins = V.fromList [0.1, 0.2, 0.3, 0.4] }

unfairCoinModel :: Fractional f => CoinModel f
unfairCoinModel = CoinModel { coins = V.fromList [0.5, 0.4, 0.75, 0.5] }

generateData :: (Variate f, Floating f, Ord f) => CoinModel f -> IO (V.Vector (V.Vector Int, Bool))
generateData (CoinModel vecs) = do
  rng <- createSystemRandom
  V.replicateM 15000 $ do
    selected_coins <- V.replicateM (V.length vecs `div` 2) $ uniformR (0, V.length vecs-1) rng
    throws <- for selected_coins $ \selected_coin -> do
      r <- uniformR (0.0, 1.0) rng
      return $ r < vecs V.! selected_coin
    return $ if all (==True) throws
      then (selected_coins, True)
      else (selected_coins, False)

-- | Given a coin model and data, returns probability that the coin model would
-- produce that set.
coinModelProducesData :: (Fractional f, Ord f) => CoinModel f -> (V.Vector (V.Vector Int, Bool)) -> ProbabilityT f Happens
coinModelProducesData coinmodel (V.toList -> coindata) =
  allHappen $ flip fmap coindata $ \(selected_coins, correct_answer) -> do
    let m = coinModel coinmodel selected_coins

    if correct_answer
      then m
      else doesntHappen m

-- | Given a coin model, return probability that all coins are heads.
coinModel :: Num f => CoinModel f -> V.Vector Int -> ProbabilityT f Happens
coinModel coinmodel selected_coins =
  allHappen $ V.toList $
    flip fmap selected_coins $ \selected_coin -> bernoulli (coins coinmodel V.! selected_coin)

perturb :: (Ord a, Traversable f, Variate a, Fractional a, PrimMonad m) => f a -> Gen (PrimState m) -> m (f a)
perturb structure rng = for structure $ \val -> do
  p <- uniformR (-0.05, 0.05) rng
  let new_val = p+val

  return $ max 0.0 $ min 1.0 new_val

-- | Optimizes a coin model.
optimize :: (Ord f, Fractional f, RealFloat f, Show f, Variate f) => CoinModel f -> (V.Vector (V.Vector Int, Bool)) -> IO ()
optimize initial_model coindata = do
  let initial_prob = probability $ coinModelProducesData initial_model coindata
  rng <- createSystemRandom
  loop_it initial_prob initial_model rng
 where
  loop_it old_probability model rng = do
    print (model, old_probability)

    perturbed_model <- perturb model rng
    let new_prob = probability $ coinModelProducesData perturbed_model coindata

    if new_prob > old_probability
      then loop_it new_prob perturbed_model rng
      else loop_it old_probability model rng


