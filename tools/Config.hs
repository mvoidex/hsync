module Config (
	Config(..),
	defConfig,
	defConfigPath
	) where

import Data.Aeson
import System.Directory
import System.FilePath

data Config = Config

instance ToJSON Config where
	toJSON cfg = object []

instance FromJSON Config where
	parseJSON = withObject "config" $ \_ → pure Config

-- | Default config
defConfig ∷ Config
defConfig = Config

-- | Default config path
defConfigPath ∷ IO FilePath
defConfigPath = do
	home ← getHomeDirectory
	return $ home </> ".hsync"
