{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}

module Numeric.Probability.Monad
  ( happensProbability
  , Prob(..)
  , runProbabilityT
  , ProbF(..)
  , ProbabilityT
  , Happens(..)
  , bernoulli
  , allHappen
  , atLeastOneHappens
  , bothHappen
  , eitherHappen
  , doesntHappen
  , probability
  , probabilityRaw )
  where

import Control.Monad.Free
import Data.Data
import Data.Foldable
import Data.Traversable
import GHC.Generics
import Numeric.LogBase

data Prob a
  = Constant a
  | Bernoulli a (Prob a) (Prob a)
  | DoesntHappen (Prob a)
  | BothHappen (Prob a) (Prob a)
  | EitherHappens (Prob a) (Prob a)
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Functor, Foldable, Traversable )

happensProbability :: Num a => Prob a -> a
happensProbability (Constant prob) = prob
happensProbability (Bernoulli prob p1 p2) =
  (prob*happensProbability p1) +
  ((1-prob)*happensProbability p2)
happensProbability (BothHappen p1 p2) = happensProbability p1 * happensProbability p2
happensProbability (EitherHappens p1 p2)
  = 1 - ((1 - happensProbability p1) * (1 - happensProbability p2))
happensProbability (DoesntHappen p1) = 1 - happensProbability p1

data ProbF f a
  = ConstantF f a
  | DoesntHappenF a
  | BernoulliF f a a
  | BothHappenF a a
  | EitherHappensF a a
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Functor, Foldable, Traversable )

type ProbabilityT f a = Free (ProbF f) a

data Happens
  = Happened
  | DidntHappen
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

allHappen :: Num f => [ProbabilityT f Happens] -> ProbabilityT f Happens
allHappen [] = return Happened
allHappen [item] = item
allHappen (x:rest) = bothHappen x (allHappen rest)

atLeastOneHappens :: Num f => [ProbabilityT f Happens] -> ProbabilityT f Happens
atLeastOneHappens [] = return Happened
atLeastOneHappens [item] = item
atLeastOneHappens (x:rest) = eitherHappen x (atLeastOneHappens rest)

bothHappen :: ProbabilityT f Happens -> ProbabilityT f Happens -> ProbabilityT f Happens
bothHappen p1 p2 = Free $ BothHappenF p1 p2

eitherHappen :: ProbabilityT f Happens -> ProbabilityT f Happens -> ProbabilityT f Happens
eitherHappen p1 p2 = Free $ EitherHappensF p1 p2

bernoulliB :: f -> ProbabilityT f Happens -> ProbabilityT f Happens -> ProbabilityT f Happens
bernoulliB probability branch1 branch2 =
  Free $ BernoulliF probability branch1 branch2

bernoulli :: Num f => f -> ProbabilityT f Happens
bernoulli probability = bernoulliB probability (return Happened) (return DidntHappen)

doesntHappen :: ProbabilityT f Happens -> ProbabilityT f Happens
doesntHappen prob = Free $ DoesntHappenF prob

probability :: RealFloat f => ProbabilityT f Happens -> LB f
probability action =
  let paction = fmap toLogBase $ runProbabilityT action
   in happensProbability paction
{-# INLINE probability #-}

probabilityRaw :: Num f => ProbabilityT f Happens -> f
probabilityRaw action = happensProbability $ runProbabilityT action
{-# INLINE probabilityRaw #-}

runProbabilityT :: Num f => ProbabilityT f Happens -> Prob f
runProbabilityT (Pure Happened) = Constant 1
runProbabilityT (Pure DidntHappen) = Constant 0
runProbabilityT (Free inside) = case inside of
  ConstantF v next -> runProbabilityT next
  BernoulliF v b1 b2 -> Bernoulli v (runProbabilityT b1) (runProbabilityT b2)
  BothHappenF b1 b2 -> BothHappen (runProbabilityT b1) (runProbabilityT b2)
  DoesntHappenF b1 -> DoesntHappen (runProbabilityT b1)
  EitherHappensF b1 b2 -> EitherHappens (runProbabilityT b1) (runProbabilityT b2)

