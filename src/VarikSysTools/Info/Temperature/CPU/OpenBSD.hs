-- | Module:    VarikSysTools.Info.Temperature.CPU.OpenBSD
-- Decription:  le filgau be lo nu zgana lo se kelvo be lo rucyca'a be
--              lo se samcmu be la'oi .OpenBSD.
-- Copyright:   (c) Varik Valefor 2022
-- License:     Unlicense
-- Maintainer:  varikvalefor@aol.com
-- Stability:   experimental
-- Portability: portable
--
-- ni'o le ti me'oi .module. cu vasru ko'a goi la'oi
-- .'cpuTempsOpenBSD'. .e le fancu poi pagbu ko'a
module VarikSysTools.Info.Temperature.CPU.OpenBSD (cpuTempsOpenBSD) where
import Text.Read;
import Data.Maybe;
import System.Info;
import Control.Monad;
import Data.List.Split;
import Data.Scientific;
import VarikSysTools.Base;
import VarikSysTools.Sysctl;

-- | ni'o ganai ko'a goi lo skami poi vasru lo ganse poi se mapti la'oi
-- .OpenBSD. .e la'oi .@cpuTempsOpenBSD. gi'e se samcmu la'oi .OpenBSD.
-- cu co'e zo'oi. @cpuTempsOpenBSD@. gi la'oi .@cpuTempsOpenBSD@. me'oi
-- .return. lo liste be lo se kelvo be lo rucyca'a poi se pilno ko'a
cpuTempsOpenBSD :: IO (Either ErrorCode [Scientific]);
cpuTempsOpenBSD = (>>= sequence . map stringToKelvin) <$> scout;

-- | ni'o gonai su'o da poi se sinxa lo me'oi .'Scientific'. zo'u ge
-- la'oi .@k@. konkatena fi du bu da gi la'o zoi. @possiblyRead k@ .zoi
-- me'oi .'Right'. me'oi .'Scientific'. da gi me'oi .'Left'. skicu lo nu
-- la'oi .@possiblyRead@. fliba
possiblyRead :: String
             -> Either ErrorCode Scientific;
possiblyRead t = maybe (Left $ cong ++ failMsg) reed $ secondBitM t
  where {
    cong = viet ++ "possiblyRead: ";
    secondBitM = junkSpace <=< listToMaybe . drop 1 . splitOn "=";
    junkSpace = listToMaybe . splitOn " ";
    reed = maybe (Left $ cong ++ noParse) Right . readMaybe;
    noParse = "The parsing of value " ++ show t ++ " fails.";
    failMsg = "Input " ++ show t ++ " apparently lacks an equals \
              \sign or some shit.";
};

-- | ni'o go ge snada lo nu me'oi .parse. la'oi .@j@. gi la'o zoi.
-- @stringToKelvin j@ .zoi me'oi .'Right'. lo sumji be lo namcu poi se
-- sinxa la'oi .@j@. bei li rezecipipamu gi la'o zoi. @stringToKelvin j@
-- .zoi me'oi .'Left'. lo cusku le du'u na me'oi .parse. la'oi .@j@.
stringToKelvin :: String -> Either ErrorCode Scientific;
stringToKelvin = fmap (+ 273.15) . possiblyRead;

-- | ni'o ro da poi velski be lo nu la'oi .@cpuTempsOpenBSD@. fliba zo'u
-- la'oi .@viet@. lidne pe'aru'e da gi'e indika le du'u da se me'oi
-- .output. la'oi .@cpuTempsOpenBSD@.
viet :: ErrorCode;
viet = "cpuTempsOpenBSD: ";

-- | ni'o ro da poi me'oi .@scout@. zo'u gonai da me'oi .'IO'. me'oi
-- .'Right'. lo'i .orsi be li re bei lo cmene be lo mu'oi zoi.
-- @sysctl(8)@ .zoi snicne goi ko'a be'o bei lo se snicne be ko'a gi da
-- me'oi .'IO'. me'oi .'Left'. lo velski be lo nu me'oi .@scout@. fliba
scout :: IO (Either ErrorCode [String]);
scout = sysctlMatching ["sensor", "temp", "cpu"];
