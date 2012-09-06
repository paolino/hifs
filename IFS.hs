
{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, MultiParamTypeClasses,EmptyDataDecls #-}

---------------------------------------------------------------------------
-- |
-- Module      :  IFS
-- Copyright   :  (c) paolo veronelli, 2012
-- License     :  BSD-style 
-- 
-- Maintainer  :  paolo.veronelli@gmail.com
-- Stability   :  experimental
-- Portability :  not portable (GADTs, MultiParamTypeClasses)
--
-- Description
--
--	  Iterated Function Systems in Haskell
--
-----------------------------------------------------------------------------
-- | An IFS is a set of weighted values which can be sampled. IFSs form a monoid for composition. A supplementary class 'Transform' is exposed to have the classicalstream of point from the weighted iteration.
module IFS (
        -- * Types
        IFS
        , Normalized
        , UnNormalized
        -- * IFS creation functions
        , ifunc
        -- * probability change
        , (?)
        -- * IFS normalization
        , normalize
        , unNormalize
        
        -- * IFS running
        , pick 
        , Transform (..)
        , points
        , pointsIO
        , module Data.Monoid
        , module System.Random

 )

 where
import Data.Monoid (Monoid (..))
import Data.List (mapAccumL)
import Control.Arrow (first, second)
import System.Random

-- | tag for normalized ifs
data Normalized
-- | tag for unnormalized ifs
data UnNormalized

-- | An IFS wraps a weighted list and can be unnormalized or normalized 
data IFS b a where
        Normalized :: [(a,Double)] -> IFS Normalized a
        UnNormalized :: [(a,Double)] -> IFS UnNormalized a

instance Functor (IFS b) where
        f `fmap` Normalized x = Normalized $ (first f) `fmap` x
        f `fmap` UnNormalized x = UnNormalized $ (first f) `fmap` x

-- | lose normalized state
unNormalize :: IFS Normalized a -> IFS UnNormalized a
unNormalize (Normalized x) = UnNormalized x

-- | gain a normalized state
normalize :: IFS UnNormalized a -> IFS Normalized a
normalize (UnNormalized l) = Normalized $ map (divideBy total) l where
	total = sum . map snd $ l
	divideBy x (a,p) = (a,p/x)

instance Monoid (IFS Normalized a) where
        Normalized la `mappend` Normalized lb = normalize $ UnNormalized (la `mappend` lb)
        mempty = Normalized []

instance Monoid (IFS UnNormalized a) where
        UnNormalized la `mappend` UnNormalized lb = UnNormalized (la `mappend` lb)
        mempty = UnNormalized []


-- Multiply IFS probabilities
change p = second (* p)

infixl 6 ?

-- | multiply all ifs probability
(?)     :: Double       -- ^ factor
        -> IFS b a      -- ^ ifs
        -> IFS UnNormalized a -- ^ new unnormalized ifs
p ? (UnNormalized la) = UnNormalized $ map (change p) la 	
p ? (Normalized la) = UnNormalized $ map (change p) la 

-- | create a normalized one function ifs
ifunc :: a -> IFS Normalized a
ifunc x = Normalized [(x,1)]

-- | select an element of the ifs given a number between 0 and 1
pick :: IFS Normalized a -> Double -> a
pick (Normalized ls) q = fst . head . dropWhile ((> 0) . snd) . snd $ mapAccumL (\q (x,p) -> (q - p, (x,q - p))) q $ ls


-- | a class for transformations
class Transform b c where
        (°) :: b -> c -> c 

instance Transform (a -> a) a where
        f ° x = f x
-- | sample driven point creation from a starting point and an ifs 
points  :: Transform b c 
        => IFS Normalized b     -- ^ generating ifs
        -> c                    -- ^ first point
        -> [Double]             -- ^ stream of random samples on the (0,1) interval
        -> [c]                  -- ^ point stream
points ifs x ds = scanl (flip (°)) x $ map (pick ifs) ds

pointsIO :: Transform b c 
        => IFS Normalized b     -- ^ generating ifs
        -> c                    -- ^ first point
        -> IO [c]                  -- ^ point stream

pointsIO ifs x = points ifs x `fmap` randomRs (0,1) `fmap` newStdGen 

