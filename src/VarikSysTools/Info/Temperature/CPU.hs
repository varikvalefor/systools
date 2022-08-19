-- | Module:    VarikSysTools.Base
-- Decription:  le filgau be lo nu zgana lo se kelvo be lo rucyca'a
-- Copyright:   (c) Varik Valefor 2022
-- License:     Unlicense
-- Maintainer:  varikvalefor@aol.com
-- Stability:   experimental
-- Portability: portable
--
-- = la .lojban.
--
-- ni'o le ti me'oi .module. cu vasru ko'a goi la'oi .'cpuTemps'.
--
-- = English
--
-- This module contains 'cpuTemps'.
module VarikSysTools.Info.Temperature.CPU (cpuTemps) where
import System.Info;
import Data.Scientific;
import VarikSysTools.Base;

import VarikSysTools.Info.Temperature.CPU.OpenBSD;

-- | = la .lojban.
--
-- ni'o go ko'a goi la'oi .'cpuTemps'. me'oi .support. lo samcmu be
-- ko'e goi lo ciste poi se co'e la'oi .'cpuTemps'. gi ko'a me'oi
-- .'Right'. me'oi .return. lo liste be lo se kelvo be lo rucyca'a poi
-- se pilno ko'e
--
-- .i go ko'a na me'oi .support. lo samcmu be ko'e gi ko'a me'oi
-- .'Left'. me'oi .return. lo velcki be lo nu ko'a na me'oi .support. lo
-- samcmu be ko'e
--
-- = English
--
-- If 'cpuTemps' supports the operating system of the system on which
-- 'cpuTemps' is run, then 'cpuTemps' 'Right'ly returns a list of the
-- kelvin-based temperatures of the processors of this system.
--
-- If 'cpuTemps' does not support the operating system of the system on
-- which 'cpuTemps' is run or something 'splodes, then 'cpuTemps'
-- returns a 'Left' 'ErrorCode' which describes the problem.
cpuTemps :: IO (Either ErrorCode [Scientific]);
cpuTemps = case os of
  "openbsd" -> cpuTempsOpenBSD
  _         -> pure $ Left $ "cpuTemps does not support " ++ os ++ ".";
