type Option = String
type Builder a = [Option] -> a
data Config = Options [Option] deriving (Show)

defaultConfig :: Builder Config
defaultConfig options = Options $ ["-Wall"] ++ options

--profile :: ([Option] -> Config) -> Config
profile :: Builder a -> a
profile builder = builder ["-prof", "-auto-all"]

--goFaster :: ([Option] -> Config) -> Config
goFaster :: Builder a -> a
goFaster builder = builder ["-O2"]

--extract :: ([Option] -> Config) -> Config
extract :: Builder a -> a
extract builder = builder []

--extend :: (([Option] -> Config) -> Config) -> ([Option] -> Config) -> ([Option] -> Config)
extend :: (Builder a -> b) -> Builder a -> Builder b
extend setter builder =
  \opts2 -> setter (\opts1 -> builder $ opts1 ++ opts2)

main :: IO ()
main = do
  putStrLn $ show $ extract $ extend goFaster defaultConfig ;
  putStrLn $ show $ extract $ extend profile $ extend goFaster defaultConfig

-----------------------------------

--profile' :: ([Option] -> Config) -> ([Option] -> Config)
profile' :: Builder a -> Builder a
profile' builder =
  \options -> builder (["-prof", "-autl-all"] ++ options)

--goFaster' :: ([Option] -> Config) -> ([Option] -> Config)
goFaster' :: Builder a -> Builder a
goFaster' builder =
  \options -> builder (["-O2"] ++ options)

