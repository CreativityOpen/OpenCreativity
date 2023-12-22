{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where
import           Control.Lens
import           Data.ByteString.Lazy    as BL
import           Data.Maybe
import           Data.Text               (Text)
import           Monomer
import           Monomer.Lens
import qualified Monomer.Lens            as L
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Download
import           Network.HTTP.Download
import           System.Directory        (createDirectoryIfMissing,
                                          getHomeDirectory)
import           Types
-- Lenses
makeLenses ''AppModel

newNode :: Node
newNode  = vstack [t, label "af"]

buildUI :: Env  -> AppModel -> Node
buildUI wenv model = if (model ^. st) then widgetTree else newNode
widgetTree :: Node
widgetTree = vstack [
    spacer,
      image "./static/gimp.png",
      vstack [hstack [button "Install" DownloadApplication, spacer, button "Open" undefined, spacer, button "Uninstall" undefined]]] `styleBasic` [paddingV 125, paddingH 350]

t :: Node
t =   hstack [
        button "Overview" undefined,
        spacer,
        button "This" AppRight
        ]

handleEvent :: Env -> Node -> AppModel -> AppEvent -> [AppResponse]
handleEvent wenv node model evt = case evt of
  AppInit  -> [SetFocusOnKey "Main"]
  AppRight -> [Event AppOtherTab
              , Model $ model & st .~ False
              , SetFocusOnKey "Second"
              ]
  AppOtherTab -> [Message "animEditIn" AnimationStart]
  DownloadApplication -> [Task $ HandleDownloadApplication <$> downloadApplication]

  _ -> []


main :: IO ()
main = do
  makeCreativityDirectory
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Hello world",
      appWindowIcon "./static/gimp.png",
      appTheme darkTheme,
      appFontDef "Regular" "./static/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel True

makeCreativityDirectory :: IO ()
makeCreativityDirectory = do
  filePath <- creativityDirectory
  createDirectoryIfMissing False filePath

creativityDirectory :: IO FilePath
creativityDirectory = do
  dir <- getHomeDirectory
  let completeDir = dir <> "/Creativity"
  return completeDir

downloadApplication :: IO ()
downloadApplication  = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest "https://github.com/aferrero2707/gimp-appimage/releases/download/continuous/GIMP_AppImage-git-2.10.25-20210527-x86_64.AppImage"
  r <- httpLbs request manager
  let contents = responseBody r
  BL.writeFile "gimp.appimage" contents

