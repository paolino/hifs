
{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, MultiParamTypeClasses,EmptyDataDecls #-}

---------------------------------------------------------------------------
-- |
-- Module      :  IFS
-- Copyright   :  (c) paolo veronelli, 2012
-- License     :  BSD-style 
-- 
-- Maintainer  :  paolo.veronelli@gmail.com
-- Stability   :  experimental
-- Portability :  portable 
--
-- Description
--
--	  Iterated Function Systems in Haskell
--
-----------------------------------------------------------------------------

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
        , unNormilize
        
        -- * IFS running
        , pick 
        , Transform (..)
        , points

 )
 where
import Data.Monoid
import Data.List
import Control.Arrow

data Normalized
data UnNormalized

-- | An IFS is expressed in a [0,1]x[0,1] squares. So, the linear transforms used to build it must take that into account.
data IFS b a where
        Normalized :: [(a,Double)] -> IFS Normalized a
        UnNormalized :: [(a,Double)] -> IFS UnNormalized a

instance Functor (IFS b) where
        f `fmap` Normalized x = Normalized $ (first f) `fmap` x
        f `fmap` UnNormalized x = UnNormalized $ (first f) `fmap` x


unNormilize :: IFS Normalized a -> IFS UnNormalized a
unNormilize (Normalized x) = UnNormalized x

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


-- | Multiply IFS probabilities
change p = second (* p)

infixl 6 ?

(?) :: Double -> IFS b a -> IFS UnNormalized a
p ? (UnNormalized la) = UnNormalized $ map (change p) la 
	
p ? (Normalized la) = UnNormalized $ map (change p) la 

ifunc :: a -> IFS Normalized a
ifunc x = Normalized [(x,1)]

-- | select an element of the IFS given a number between 0 and 1
pick :: IFS Normalized a -> Double -> a
pick (Normalized ls) q = fst . head . dropWhile ((> 0) . snd) . snd $ mapAccumL (\q (x,p) -> (q - p, (x,q - p))) q $ ls

class Transform b c where
        (°) :: b -> c -> c 

points :: Transform b c => IFS Normalized b -> [Double] -> c -> [c]
points ifs ds x = scanl (flip (°)) x (map (pick ifs) ds)

		

