-- | ni'o le ti me'oi .module. cu vasru ko'a goi la'oi .'cpuTemps'.
module VarikSysTools.Info.Temperature.CPU (cpuTemps) where
import System.Info;
import Data.Scientific;
import VarikSysTools.Base;

import VarikSysTools.Info.Temperature.CPU.OpenBSD;

-- | ni'o go ko'a goi la'oi .'cpuTemps'. me'oi .support. lo samcmu be
-- ko'e goi lo ciste poi se co'e la'oi .'cpuTemps'. gi ko'a me'oi
-- .'Right'. me'oi .return. lo liste be lo se kelvo be lo rucyca'a poi
-- se pilno ko'e
--
-- .i go ko'a na me'oi .support. lo samcmu be ko'e gi ko'a me'oi
-- .'Left'. me'oi .return. lo velcki be lo nu ko'a na me'oi .support. lo
-- samcmu be ko'e
cpuTemps :: IO (Either ErrorCode [Scientific]);
cpuTemps = case os of
  "openbsd" -> cpuTempsOpenBSD
  _         -> pure $ Left $ "cpuTemps does not support " ++ os ++ ".";
