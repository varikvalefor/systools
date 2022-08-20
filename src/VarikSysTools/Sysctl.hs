-- | Module:    VarikSysTools.Base
-- Decription:  le fancu poi filgau lo nu pilno la'o gy. sysctl(8) .gy
--              / Functions what facilitate the use of sysctl(8)
-- Copyright:   (c) Varik Valefor 2022
-- License:     Unlicense
-- Maintainer:  varikvalefor@aol.com
-- Stability:   experimental
-- Portability: portable
--
-- = la .lojban.
--
-- ni'o le ti me'oi .module. cu vasru la'oi .'sysctlMatching'. je le
-- fancu poi se pilno la'oi .'sysctlMatching'.
--
-- = English
--
-- This module contains 'sysctlMatching' and the functions whuich are
-- used by 'sysctlMatching'.
module VarikSysTools.Sysctl (sysctlMatching) where
import System.Exit;
import System.Process;
import VarikSysTools.Base;

-- \| = la .lojban.
--
-- ni'o go la'o zoi. @nfx a b@ .zoi me'oi .'True'. gi la'oi .@b@.
-- vasru la'oi .@a@.
--
-- = English
--
-- @nfx a b@ if and only if @a@ is an infix of @b@.
nfx :: Eq a => [a] -> [a] -> Bool;
nfx _ [] = False;
nfx a b = take (length a) b == a || nfx a (drop 1 b);

-- | = la .lojban.
--
-- ni'o gonai ge ko'a goi la'o zoi. @sysctlMatching t@ .zoi na fliba
-- gi ko'a me'oi .'IO'. me'oi .'Right'. lo'i lerpinsle poi se me'oi
-- .output. la'o zoi. @sysctl(8)@ .zoi je cu vasru lo ro se vasru be
-- la'oi .@t@. gi ko'a me'oi .'IO'. me'oi .'Left'. lo velski be lo nu
-- ko'a fliba
--
-- = English
--
-- If @sysctlMatching t@ does not fail, then @sysctlMatching t@ is an
-- IO-monadic 'Right' list of the lines which are output by sysctl(8)
-- and contain @t@.  If @sysctlMatching t@ fails, then @sysctlMatching
-- t@ returns a 'Left' 'ErrorCode' which describes @sysctlMatching@'s
-- failure.
sysctlMatching :: [String]
               -> IO (Either ErrorCode [String]);
sysctlMatching f = fmap (filter $ \t -> all (`nfx` t) f) <$> sysctlLines
  where {
  -- \| = la .lojban.
  --
  -- ni'o la'oi .@rpwe@. selsmudu'i la'oi .@readProcessWithExitCode@. .i
  -- lo nu me'oi .abbreviate. cu filri'a lo nu ciska lo mu'oi glibau.
  -- one-liner .glibau kei .e lo nu na ciska lo du'e fancu ja snicne
  --
  -- = English
  --
  -- 'rpwe' is a synonym of 'readProcessWithExitCode'.  Abbreviating
  -- facilitates writing one-liners and not writing excessively many
  -- functions and variables.
  rpwe :: FilePath
       -> [String]
       -> String
       -> IO (ExitCode, String, String);
  rpwe = readProcessWithExitCode;

  -- \| = la .lojban.
  --
  -- ni'o gonai ge la'oi .@t@. indika lo du'u fliba gi la'o zoi.
  -- @toEither t@ .zoi me'oi .'Left'. skicu lo nu fliba gi la'o zoi.
  -- @toEither t@ .zoi me'oi .'Right'. te .orsi la'oi .@t@.
  --
  -- = English
  --
  -- If @t@ indicates failure, then @toEither t@ is a 'Left' description
  -- of the failure.  If @t@ indicates that everything is 'Right', then
  -- @toEither t@ returns the first element of @t@.
  toEither :: (ExitCode, String, String)
           -> Either ErrorCode String;
  toEither (ExitFailure t, a, b) = Left m
    where {
    m = "sysctlMatching: sysctl reports exit code " ++ show t ++
        "and outputs " ++ show a ++ " and " ++ show b ++ " to the \
        \standard output and standard error, respectively.";
  };
  toEither (_, a, _) = Right a;
};

-- | = la .lojban.
--
-- ni'o gonai ge ko'a goi la'oi .@sysctlLines@. fliba gi skicu lo
-- nu ko'a fliba gi lo'i lerpinsle poi se me'oi .output. la'o zoi.
-- @sysctl(8) .zoi
--
-- = English
--
-- If @sysctlLines@ fails, then a 'Left' description of
-- @sysctlLines@'s failure is returned.  If @sysctlLines@ does not
-- fail, then @sysctlLines@ returns a 'Right' list of the lines which
-- are output by sysctl(8).
sysctlLines :: IO (Either ErrorCode [String]);
sysctlLines = fmap lines . toEither <$> rpwe "sysctl" [] [];
