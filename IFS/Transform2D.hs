{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveFunctor #-}
---------------------------------------------------------------------------
-- |
-- Module      :  Transform2D
-- Copyright   :  (c) alpheccar 2007, paolo veronelli 2012
-- License     :  BSD-style 
-- 
-- Maintainer  :  paolo.veronelli@gmail.com
-- Stability    : experimental
-- Portability  : non-portable (multi-parameter type classes)
--
-- Description
--
--	  Some geometry operations and instances of the IFS
--
-----------------------------------------------------------------------------

module IFS.Transform2D (
   -- * Types
   -- ** Matrix and Vector
   L
 , V(..)
   -- ** Non linear transformations
 , D (..)
 , DL (..)
  -- ** Transforming and running
 , module IFS
   -- * Creating linear transformations
 , linear
 , rotation
 , scaling
 , translation
  -- * Non linear transformations (alpheccar contribution)
 , v1
 , v2
 , v3
 , v4
 , v5
 , v6
 , v7
 , v8
 , v9
 , v10
 , v11
 , v12
 )
 where

import IFS (Transform (..), IFS, Normalized, points)
import Prelude hiding ((.))
import Control.Category ((.), Category)

-- | Vector
newtype V a = V {unV :: (a,a)} deriving(Eq,Show,Functor,Ord)

-- | A direct transformation
newtype D a = D {unD :: V a -> V a} 

-- | Affine transform on 2x2 space
newtype L a = L (a,a,a,a,a,a) deriving(Eq,Show)

-- | A direct transformation with an affine one
newtype DL a = DL {unDL ::(D a,L a) }


instance Num a => Transform (L a) (V a) where
	L (a,b,c,d,e,f) ° V (x,y) = V (a*x+b*y + e,c*x+d*y + f)
instance Num a => Transform (L a) (L a) where
        L (a,b,c,d,e,f) ° L (a',b',c',d',e',f') = L (a*a' + b*c',a*b' + b*d',c*a' + d*c',c*b' + d*d',a*e' + b*f' + e,c*e' + d*f' + f)

instance Transform (D a) (V a)  where
	D f ° v = f v
instance Transform (D a) (D a)  where
	D f ° D g = D (f . g)

instance Num a => Transform (DL a) (V a) where
	(°) (DL (f,m)) v = f ° (m ° v)	

instance Transform (D a) (DL a) where
	f ° (DL (g,m)) = DL (f ° g,m)

instance Num a => Transform (L a) (DL a)  where
	f ° (DL (g,m)) = DL (g, f ° m)

instance Num a => Transform (DL a) (DL a)  where
	(DL (f,m')) ° (DL (g,m)) = DL (f ° g, m' ° m)

-- | Create a pure affine transformation
-- Linear part:
-- a b
-- c d
-- Affine part:
-- e
-- f
linear :: Num a => a -- ^ a
       -> a -- ^ b
       -> a -- ^ c
       -> a -- ^ d 
       -> a -- ^ e 
       -> a -- ^ f
       -> L a
linear a b c d e f = L (a,b,c,d,e,f)


-- | Sinusoidal
v1 :: D Double
v1 = D f where 
	f (V(x,y))  = V(sin x,sin y)

-- | Spherical
v2 :: D Double
v2 = D f where 
	f (V(x,y))  = V(x/(r2+1e-6),y/(r2+1e-6))
         where
                r2 = x*x + y*y

-- | Swirl	
v3 :: D Double
v3 = D f where 
	f (V(x,y))  = V(r*cos(theta+r),r*sin(theta+r))
          where
                theta = atan(x/y)
                r = sqrt(x*x + y*y)

-- | Horseshoe	
v4 :: D Double
v4 = D f where 
	f (V(x,y))  = V(r*cos(2*theta),r*sin(2*theta))
          where
                theta = atan(x/y)
                r = sqrt(x*x + y*y)

-- | Polar
v5 :: D Double
v5 = D f where 
	f (V(x,y))  = V(theta/pi,r-1)
          where
                theta = atan(x/y)
                r = sqrt(x*x + y*y)
	
-- | Handkerchief
v6 :: D Double
v6 = D f where 
	f (V(x,y))  = V(r*sin(theta+r),r*cos(theta-r))
          where
                theta = atan(x/y)
                r = sqrt(x*x + y*y)
	
-- | Heart
v7 :: D Double
v7 = D f where 
	f (V(x,y))  = V(r*sin(theta*r),-r*cos(theta*r))
          where
                theta = atan(x/y)
                r = sqrt(x*x + y*y)
	
-- | Disc
v8 :: D Double
v8 = D f where 
	f (V(x,y))  = V(theta*sin(pi*r)/pi,theta*cos(pi*r)/pi)
          where
                theta = atan(x/y)
                r = sqrt(x*x + y*y)

-- | Spiral
v9 :: D Double
v9 = D f where 
	f (V(x,y))  = V(((cos theta) + (sin r))/r,((sin theta)-(cos r))/r)
          where
                theta = atan(x/y)
                r = sqrt(x*x + y*y)

-- | Hyperbolic
v10 :: D Double
v10 = D f where 
	f (V(x,y))  = V(sin(theta)/r,cos(theta)*r)
          where
                theta = atan(x/y)
                r = sqrt(x*x + y*y)

-- | Diamond
v11 :: D Double
v11 = D f where 
	f (V(x,y))  = V((sin theta)*(cos r),(cos theta)*(sin r))
          where
                theta = atan(x/y)
                r = x*x + y*y

-- | Ex
v12 :: D Double
v12 = D f where 
	f (V(x,y))  = V(r*(sin(theta+r))^3,r*(cos(theta-r))^3)
          where
                theta = atan(x/y)
                r = sqrt(x*x + y*y)

rotation :: Double -> L Double
rotation t = L (cos (t*pi/180),sin (t*pi/180),-sin (t*pi/180),cos (t*pi/180),0,0)

scaling :: Double -> Double -> L Double
scaling sx sy = L (sx,0,0,sy,0,0)

translation :: Double -> Double -> L Double
translation tx ty = L (1,0,0,1,tx,ty)

