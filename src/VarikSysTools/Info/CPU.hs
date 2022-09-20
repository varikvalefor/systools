{-# LANGUAGE CApiFFI #-}

-- | Module:    VarikSysTools.Base
-- Decription:  le fancu poi filgau lo nu djuno fi lo rucyca'a
--              / Functions which facilitate understanding CPUs
-- Copyright:   (c) Varik Valefor 2022
-- License:     Unlicense
-- Maintainer:  varikvalefor@aol.com
-- Stability:   experimental
-- Portability: POSIX
--
-- = la .lojban.
--
-- ni'o le me'oi .module. cu vasru le fancu poi filgau lo nu djuno
-- zo'e pe lo rucyca'a
--
-- = English
--
-- This module contains functions which facilitate understanding stuff
-- regarding the processor.
module VarikSysTools.Info.CPU where

import Data.Bool;
import Foreign.Ptr;
import Foreign.ForeignPtr;
import Foreign.Marshal.Alloc;
import qualified Data.Vector.Storable as S;

foreign import capi unsafe "getloadavg" getloadavg :: Ptr Double
                                                   -> Int
                                                   -> IO Int;

-- | = la .lojban.
--
-- ni'o gonai ge snada gi la'oi ko'a goi .@get3LoadAvg@. me'oi .'IO'. lo
-- me'oi .'Right'. be lo .orsi be li ci bei lo mentu mu'oi gliau. load
-- average .glibau. bei lo relme'u mu'oi glibau. load average
-- .glibau. bei lo cibme'u mu'oi glibau. load average .glibau. gi ko'a
-- me'oi .'IO'. lo me'oi .'Left'. be lo fliba namcu
--
-- = English
--
-- If everything works, then @get3LoadAvg@ returns a 'Right' 3-list of
-- the 1-minute load average of the processor, the 5-minute load average
-- of the processor, and the 15-minute load average of the procesor.  If
-- something fails, then @get3LoadAvg@ returns a 'Left' error code.
get3LoadAvg :: IO (Either Int [Double]);
get3LoadAvg = wPtr $ \a -> proc =<< flip toEither a =<< getloadavg a 3
  where {
  wPtr :: (Ptr a -> IO b) -> IO b;
  wPtr = allocaBytes 0;
  --
  toEither :: Num a => Eq a => a -> b -> IO (Either a b);
  toEither a b = pure $ bool (Left a) (Right b) $ a /= (-1);
  --
  proc :: Either Int (Ptr Double) -> IO (Either Int [Double]);
  proc = either (pure . Left) (fmap Right . ptrToList);
  --
  ptrToList :: Ptr Double -> IO [Double];
  ptrToList = fmap (S.toList . toVector) . newForeignPtr_;
  --
  toVector :: ForeignPtr a -> S.Vector a;
  toVector = flip S.unsafeFromForeignPtr0 3;
};
