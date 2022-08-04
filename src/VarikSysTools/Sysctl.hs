module VarikSysTools.Sysctl (sysctlMatching) where
import System.Exit;
import System.Process;
import VarikSysTools.Base;

-- \| ni'o go la'o zoi. @nfx a b@ .zoi me'oi .'True'. gi la'oi .@b@.
-- vasru la'oi .@b@.
nfx :: Eq a => [a] -> [a] -> Bool;
nfx _ [] = False;
nfx a b = take (length a) b == a || nfx a (drop 1 b);

-- | ni'o gonai ge ko'a goi la'o zoi. @sysctlMatching t@ .zoi na fliba
-- gi ko'a me'oi .'IO'. me'oi .'Right'. lo'i lerpinsle poi se me'oi
-- .output. la'o zoi. @sysctl(8)@ .zoi gi'e vasru lo ro se vasru be
-- la'oi .@t@. gi ko'a me'oi .'IO'. me'oi .'Left'. lo velski be lo nu
-- ko'a fliba
sysctlMatching :: [String]
               -> IO (Either ErrorCode [String]);
sysctlMatching f = fmap (filter $ \t -> all (`nfx` t) f) <$> sysctlLines
  where {
    -- \| ni'o gonai ge ko'a goi la'oi .@sysctlLines@. fliba gi skicu lo
    -- nu ko'a fliba gi lo'i lerpinsle poi se me'oi .output. la'o zoi.
    -- @sysctl(8) .zoi
    sysctlLines :: IO (Either ErrorCode [String]);
    sysctlLines = fmap lines . toEither <$> rpwe "sysctl" [] [];

    -- \| ni'o la'oi .@rpwe@. selsmudu'i la'oi
    -- .@readProcessWithExitCode@. .i lo nu me'oi .abbreviate. cu
    -- filri'a lo nu ciska lo mu'oi glibau. one-liner .glibau kei .e lo
    -- nu na ciska lo du'e fancu ja snicne
    rpwe :: FilePath
         -> [String]
         -> String
         -> IO (ExitCode, String, String);
    rpwe = readProcessWithExitCode;

    -- \| ni'o gonai ge la'oi .@t@. indika lo du'u fliba gi la'o zoi.
    -- @toEither t@ .zoi me'oi .'Left'. skicu lo nu fliba gi la'o zoi.
    -- @toEither t@ .zoi me'oi .'Right'. te .orsi be la'oi .@t@.
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
