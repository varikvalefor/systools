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

-- | = la .lojban.
--
-- ni'o ganai ko'e goi lo skami poi se co'e la'oi .'cpuTempsOpenBSD'.
-- cu se samcmu la'oi .OpenBSD. gi gonai ge la'oi .'cpuTempsOpenBSD'.
-- me'oi .return. me'oi .'Right'. lo'i se kelvo be lo rucyca'a be ko'e
-- gi me'oi .return. lo me'oi .'Left'. me'oi .'ErrorCode'. poi skicu lo
-- nu fliba
--
-- = English
--
-- If the system on which 'cpuTempsOpenBSD' is run uses OpenBSD and
-- sysctl(8) supports the temperature sensors of this system, then
-- 'cpuTempsOpenBSD' 'Right'ly outputs a list of the kelvin-based
-- temperatures of the processors of this system.  'cpuTempsOpenBSD'
-- otherwise returns a 'Left' 'ErrorCode' which describes any problem
-- which is encountered.
cpuTempsOpenBSD :: IO (Either ErrorCode [Scientific]);
cpuTempsOpenBSD = (>>= sequence . map stringToKelvin) <$> scout;

-- | = la .lojban.
--
-- ni'o gonai su'o da poi se sinxa lo me'oi .'Scientific'. zo'u ge
-- la'oi .@k@. konkatena fi du bu da gi la'o zoi. @possiblyRead k@ .zoi
-- me'oi .'Right'. me'oi .'Scientific'. da gi me'oi .'Left'. skicu lo nu
-- la'oi .@possiblyRead@. fliba
--
-- = English
--
-- If @k@ is the concatenation of something, "=", and a @t@, which is a
-- 'String' representation of some 'Scientific' number, then
-- @possiblyRead k@ is the 'Scientific' number which is rightly
-- represented by @t@.  @possiblyRead k@ is otherwise a 'Left'
-- 'ErrorCode' which describes @possiblyRead@'s failure.
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

-- | = la .lojban.
--
-- ni'o go ge snada lo nu me'oi .parse. la'oi .@j@. gi la'o zoi.
-- @stringToKelvin j@ .zoi me'oi .'Right'. lo sumji be lo namcu poi se
-- sinxa la'oi .@j@. bei li rezecipipamu gi la'o zoi. @stringToKelvin j@
-- .zoi me'oi .'Left'. lo cusku le du'u na me'oi .parse. la'oi .@j@.
--
-- = English
--
-- If the parsing of @t@ is successful, then @stringToKelvin t@ is the
-- sum of 273.15 and the 'Right'ly-parsed version of @t@.
-- @stringToKelvin j@ is otherwise a 'Left' 'ErrorCode' which states
-- that the parsing of @t@ fails.
stringToKelvin :: String -> Either ErrorCode Scientific;
stringToKelvin = fmap (+ 273.15) . possiblyRead;

-- | = la .lojban.
--
-- ni'o ro da poi velski be lo nu la'oi .@cpuTempsOpenBSD@. fliba zo'u
-- la'oi .@viet@. lidne pe'aru'e da gi'e indika le du'u da se me'oi
-- .output. la'oi .@cpuTempsOpenBSD@.
--
-- = English
--
-- For all error messages which are output by @cpuTempsOpenBSD@ @t@,
-- @t@ is prepended to @t@, indicating that @cpuTempsOpenBSD@ is
-- responsible for @t@.
viet :: ErrorCode;
viet = "cpuTempsOpenBSD: ";

-- | = la .lojban.
--
-- ni'o ro da poi me'oi .@scout@. zo'u gonai da me'oi .'IO'. me'oi
-- .'Right'. lo'i .orsi be li re bei lo cmene be lo mu'oi zoi.
-- @sysctl(8)@ .zoi snicne goi ko'a be'o bei lo se snicne be ko'a gi da
-- me'oi .'IO'. me'oi .'Left'. lo velski be lo nu me'oi .@scout@. fliba
--
-- = English
--
-- For all @scout@ @t@, @t@ is either an IO-monadic 'Right' list of
-- sysctl(8) outputs or an IO-monadic 'Left' 'ErrorCode' which explains
-- @scout@'s failure.
scout :: IO (Either ErrorCode [String]);
scout = sysctlMatching ["sensor", "temp", "cpu"];
