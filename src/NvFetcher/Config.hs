module NvFetcher.Config where

import Data.Default
import Development.Shake

-- | Nvfetcher configuration
data Config = Config
  { shakeConfig :: ShakeOptions,
    buildDir :: FilePath,
    customRules :: Rules (),
    actionAfterBuild :: Action (),
    actionAfterClean :: Action (),
    retry :: Int,
    filterRegex :: Maybe String
  }

instance Default Config where
  def =
    Config
      { shakeConfig =
          shakeOptions
            { shakeProgress = progressSimple,
              shakeThreads = 0
            },
        buildDir = "_sources",
        customRules = pure (),
        actionAfterBuild = pure (),
        actionAfterClean = pure (),
        retry = 3,
        filterRegex = Nothing
      }
