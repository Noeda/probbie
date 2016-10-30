{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

module Numeric.Probability.Monad
  ( pointDensityFunctionLB
  , erf
  , normalCdf
  , cauchyCdf
  , Prob(..)
  , runProbabilityT
  , ProbF(..)
  , ProbabilityT
  , Happens(..)
  , bernoulli
  , gaussian
  , gaussianIntervalB
  , gaussianInterval
  , gaussianSmallerThanOther
  , cauchy
  , cauchyIntervalB
  , cauchyInterval
  , cauchySmallerThanOther
  , allHappen
  , atLeastOneHappens
  , bothHappen
  , eitherHappen
  , doesntHappen
  , probability )
  where

import Control.Monad.Free
import Data.Data
import GHC.Generics
import Numeric.LogBase

data Prob a
  = Constant a
  | Bernoulli a (Prob a) (Prob a)
  | Cauchy a a a (Prob a)
  | CauchyInterval a a a a (Prob a) (Prob a)
  | Gaussian a a a (Prob a)
  | GaussianInterval a a a a (Prob a) (Prob a)
  | DoesntHappen (Prob a)
  | BothHappen (Prob a) (Prob a)
  | EitherHappens (Prob a) (Prob a)
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Functor, Foldable, Traversable )

erf :: Floating a => a -> a
erf x = signum x * sqrt payload
 where
  x2 = x**2
  a = (8*(pi - 3)) / (3 * pi * (4 - pi))
  payload = 1 - exp (negate $ x2 * ((4/pi) + a*x2) / (1 + a*x2))
{-# INLINE erf #-}

normalCdf :: Floating a => a -> a -> a -> a
normalCdf point mean sigma2 =
  0.5 * (1 + erf ( (point-mean) / (sqrt sigma2 * sqrt 2) ) )
{-# INLINE normalCdf #-}

cauchyCdf :: Floating a => a -> a -> a -> a
cauchyCdf point middle gamma =
  (1/pi) * atan ( (middle-point)/gamma ) + 0.5
{-# INLINE cauchyCdf #-}

pointDensityFunctionLB :: (Ord a, RealFloat a) => Prob a -> LB a
pointDensityFunctionLB (Constant prob) = toLogBase prob
pointDensityFunctionLB (Bernoulli prob p1 p2) =
  (toLogBase prob*pointDensityFunctionLB p1) +
  ((toLogBase $ 1-prob)*pointDensityFunctionLB p2)
pointDensityFunctionLB (BothHappen p1 p2) = pointDensityFunctionLB p1 * pointDensityFunctionLB p2
pointDensityFunctionLB (EitherHappens p1 p2)
  = 1 - ((1 - pointDensityFunctionLB p1) * (1 - pointDensityFunctionLB p2))
pointDensityFunctionLB (DoesntHappen p1) = 1 - pointDensityFunctionLB p1

pointDensityFunctionLB (GaussianInterval a b mean sigma2 p1 p2)
  | b <= a = 0
  | otherwise =
      let a_cdf = normalCdf a mean sigma2
          b_cdf = normalCdf b mean sigma2

          positive_prob = b_cdf - a_cdf
          negative_prob = 1.0 - positive_prob

       in if | negative_prob == 1.0 && positive_prob == 0.0
               -> pointDensityFunctionLB p2
             | negative_prob == 0.0 && positive_prob == 1.0
               -> pointDensityFunctionLB p1
             | otherwise ->
                (toLogBase positive_prob * pointDensityFunctionLB p1) +
                (toLogBase negative_prob * pointDensityFunctionLB p2)

pointDensityFunctionLB (Gaussian point mean sigma2 p1) =
  let p = (1/(sqrt (2*sigma2*pi))) * exp (negate $ ((point - mean)**2)/(2 * sigma2))
   in toLogBase p*pointDensityFunctionLB p1

pointDensityFunctionLB (Cauchy point middle gamma p1) =
  let p = (1/( (pi*gamma) * (1 + ((middle - point)/gamma)**2 ) ))
   in toLogBase p*pointDensityFunctionLB p1

pointDensityFunctionLB (CauchyInterval a b middle gamma p1 p2)
  | b <= a = 0
  | otherwise =
      let a_cdf = cauchyCdf a middle gamma
          b_cdf = cauchyCdf b middle gamma

          positive_prob = b_cdf - a_cdf
          negative_prob = 1.0 - positive_prob

       in if | negative_prob == 1.0 && positive_prob == 0.0
               -> pointDensityFunctionLB p2
             | negative_prob == 0.0 && positive_prob == 1.0
               -> pointDensityFunctionLB p1
             | otherwise ->
                (toLogBase positive_prob * pointDensityFunctionLB p1) +
                (toLogBase negative_prob * pointDensityFunctionLB p2)

{-# INLINE pointDensityFunctionLB #-}

data ProbF f a
  = ConstantF f a
  | DoesntHappenF a
  | BernoulliF f a a
  | BothHappenF a a
  | EitherHappensF a a
  | GaussianF f f f a
  | GaussianIntervalF f f f f a a
  | CauchyF f f f a
  | CauchyIntervalF f f f f a a
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

bernoulli :: f -> ProbabilityT f Happens
bernoulli probability = bernoulliB probability (return Happened) (return DidntHappen)

doesntHappen :: ProbabilityT f Happens -> ProbabilityT f Happens
doesntHappen prob = Free $ DoesntHappenF prob

probability :: RealFloat f => ProbabilityT f Happens -> LB f
probability action =
  let paction = runProbabilityT action
   in pointDensityFunctionLB paction
{-# INLINE probability #-}

gaussian :: f -> f -> f -> ProbabilityT f Happens
gaussian point mean sigma2 = Free $ GaussianF point mean sigma2 (return Happened)
{-# INLINE gaussian #-}

gaussianIntervalB :: f -> f -> f -> f -> ProbabilityT f Happens -> ProbabilityT f Happens -> ProbabilityT f Happens
gaussianIntervalB a b mean sigma2 p1 p2 = Free $ GaussianIntervalF a b mean sigma2 p1 p2
{-# INLINE gaussianIntervalB #-}

gaussianInterval :: f -> f -> f -> f -> ProbabilityT f Happens
gaussianInterval a b mean sigma2 = gaussianIntervalB a b mean sigma2 (return Happened) (return DidntHappen)
{-# INLINE gaussianInterval #-}

gaussianSmallerThanOther :: Num f => f -> f -> f -> f -> ProbabilityT f Happens
gaussianSmallerThanOther mean1 sigma12 mean2 sigma22 =
  gaussianInterval (negate (sigma12+sigma22) * 1000) 0 (mean1-mean2) (sigma12+sigma22)
{-# INLINE gaussianSmallerThanOther #-}

cauchy :: f -> f -> f -> ProbabilityT f Happens
cauchy point middle gamma = Free $ CauchyF point middle gamma (return Happened)
{-# INLINE cauchy #-}

cauchyIntervalB :: f -> f -> f -> f -> ProbabilityT f Happens -> ProbabilityT f Happens -> ProbabilityT f Happens
cauchyIntervalB a b middle gamma p1 p2 = Free $ CauchyIntervalF a b middle gamma p1 p2
{-# INLINE cauchyIntervalB #-}

cauchyInterval :: f -> f -> f -> f -> ProbabilityT f Happens
cauchyInterval a b middle gamma = cauchyIntervalB a b middle gamma (return Happened) (return DidntHappen)
{-# INLINE cauchyInterval #-}

cauchySmallerThanOther :: Num f => f -> f -> f -> f -> ProbabilityT f Happens
cauchySmallerThanOther middle1 gamma1 middle2 gamma2 =
  cauchyInterval (negate (gamma1+gamma2) * 100000000) 0 (middle1-middle2) (gamma1+gamma2)
{-# INLINE cauchySmallerThanOther #-}

runProbabilityT :: Num f => ProbabilityT f Happens -> Prob f
runProbabilityT (Pure Happened) = Constant 1
runProbabilityT (Pure DidntHappen) = Constant 0
runProbabilityT (Free inside) = case inside of
  ConstantF _v next -> runProbabilityT next
  BernoulliF v b1 b2 -> Bernoulli v (runProbabilityT b1) (runProbabilityT b2)
  BothHappenF b1 b2 -> BothHappen (runProbabilityT b1) (runProbabilityT b2)
  DoesntHappenF b1 -> DoesntHappen (runProbabilityT b1)
  EitherHappensF b1 b2 -> EitherHappens (runProbabilityT b1) (runProbabilityT b2)
  GaussianF point mean sigma2 next -> Gaussian point mean sigma2 (runProbabilityT next)
  GaussianIntervalF a b mean sigma2 p1 p2 -> GaussianInterval a b mean sigma2 (runProbabilityT p1) (runProbabilityT p2)
  CauchyF point middle gamma next -> Cauchy point middle gamma (runProbabilityT next)
  CauchyIntervalF a b middle gamma p1 p2 -> CauchyInterval a b middle gamma (runProbabilityT p1) (runProbabilityT p2)
{-# INLINE runProbabilityT #-}

