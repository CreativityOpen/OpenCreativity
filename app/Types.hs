module Types where
import           Control.Lens
import           Data.ByteString.Lazy    as BL
import           Data.Maybe
import           Data.Text               (Text)
import           Data.Text               (Text)
import           Monomer
import           Monomer
import qualified Monomer.Lens            as L
import qualified Monomer.Lens            as L
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Download
import           Network.HTTP.Download
import qualified RIO
import           System.Directory        (createDirectoryIfMissing,
                                          getHomeDirectory)
type Env = WidgetEnv AppModel AppEvent
type Node = WidgetNode AppModel AppEvent
type AppResponse = AppEventResponse AppModel AppEvent
newtype AppModel = AppModel {
  _st :: Bool
} deriving (Eq, Show)
data AppEvent
  = AppInit
  | AppRight
  | AppOtherTab
  | DownloadApplication
  | HandleDownloadApplication ()
  deriving (Eq, Show)
