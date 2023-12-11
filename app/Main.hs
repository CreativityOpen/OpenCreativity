{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Lens
import           Data.Maybe
import           Data.Text    (Text)
import           Monomer
import           TextShow

import qualified Monomer.Lens as L

newtype AppModel = AppModel {
  _st :: Bool
} deriving (Eq, Show)

makeLenses ''AppModel

data AppEvent
  = AppInit
  | AppRight
  | AppOtherTab
  deriving (Eq, Show)

type Env = WidgetEnv AppModel AppEvent
type Node = WidgetNode AppModel AppEvent
type AppResponse = AppEventResponse AppModel AppEvent


newNode :: Node
newNode  = vstack [t, label "af"]

buildUI :: Env  -> AppModel -> Node
buildUI wenv model = if (model ^. st) then widgetTree else newNode where
  widgetTree = vstack [
    spacer,
        t,
      vstack [image "./static/newgimp.png", hstack [button "Install" undefined, spacer, button "Open" undefined, spacer, button "Uninstall" undefined]] `styleBasic` [paddingV 125]
    ] `styleBasic` [paddingH 450] `nodeKey` "Main" `nodeVisible` (model ^. st)
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
  _ -> []
main :: IO ()
main = do
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
