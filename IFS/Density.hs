{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleInstances #-}

-- module IFS.Density where

import IFS
import IFS.Transform2D
import System.Random
import Data.Array.IO
import Data.Array
import Data.Array.MArray
import Data.Ord
import Data.List
import Control.Monad (when)
import Control.Concurrent
import Control.Arrow

type Density a = [(a,Int)]

deriving instance Ix (V Int)

project :: RealFrac a => a -> V a -> V Int
project ro  = fmap (round . (*ro)) 

clean :: Density a -> Density a
clean = sortBy (flip $ comparing snd) . filter ((>0) . snd) 

density :: (V Int, V Int) -> [V Int] -> Density (V Int)
density dlur  =  assocs  . accumArray (+) 0 dlur . map (flip (,) 1) . filter (inRange dlur)  where
        

densityIO :: Int -> (V Int, V Int) -> [V Int] -> IO (Density (V Int))
densityIO time dlur xs = do
        m <- newMVar False
        forkIO $ threadDelay time >> swapMVar m True >> return ()
        arr <- accumArrayBounded m (+) 0 dlur. map (flip (,) 1) . filter (inRange dlur) $ xs -- :: IO (IOArray (V Int) Int)
        assocs `fmap` unsafeFreeze arr


accumArrayBounded :: Ix i => MVar Bool -> (e -> e' -> e) -> e -> (i,i) -> [(i, e')] -> IO (IOArray i e)
accumArrayBounded stop f e (l,u) ies = do
    marr <- newArray (l,u) e
    let action [] = return ()
        action ((i,new):rs)= do
                t <- readMVar stop
                when (not t) $ do
                        old <- readArray marr i
                        writeArray marr i (f old new)
                        action rs
    action ies
    return marr

main = do 
        ps <- pointsIO (ifunc (scaling 0.5 0.4 ° rotation 90) `mappend` ifunc (translation 0.6 0.3 ° rotation 45)) (V 0 (0::Double))  
        -- zs <- fmap clean . densityIO 10000000 (V (-50) (-50), V 50 50) . map (project 16) $ ps
        let zs = normalize . wfuncs . map ((const :: V Int -> V Int -> V Int) *** fromIntegral) . clean . density  (V (-50) (-50), V 50 50) . map (project 16) $ take 100000 ps
        pointsIO zs (V 0 0 :: V Int) >>= print . take 100

