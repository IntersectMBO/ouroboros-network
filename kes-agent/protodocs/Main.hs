{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Cardano.KESAgent.Serialization.Spec
import Cardano.KESAgent.Serialization.Spec.Class
import Cardano.KESAgent.Protocols.Control.Protocol
import qualified Cardano.KESAgent.Protocols.Control.Protocol as Control
import qualified Cardano.KESAgent.Protocols.Control.Driver ()
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protodocs.TH
import Data.List

import Control.Monad
import Language.Haskell.TH
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Data.Text.Lazy.IO as LText
import Text.Printf
import Data.Maybe

renderState :: [MessageDescription] -> String -> Html
renderState msgs stateName =
  H.div ! HA.class_ "state" $ do
    H.h3 ! HA.id (H.stringValue ("state_" ++ stateName)) $ H.string stateName
    unless (null messagesFromHere) $ do
      H.h4 "Messages from here:"
      H.ul $ do
        forM_ messagesFromHere $ \msg -> do
          H.li $ do
            H.strong $
              H.a ! HA.href (H.stringValue ("#message_" ++ messageName msg)) $ H.string (messageName msg)
            " (to "
            H.a ! HA.href (H.stringValue ("#state_" ++ messageToState msg)) $ H.string (messageToState msg)
            ")"
    unless (null messagesToHere) $ do
      H.h4 "Messages to here:"
      H.ul $ do
        forM_ messagesToHere $ \msg -> do
          H.li $ do
            H.strong $
              H.a ! HA.href (H.stringValue ("#message_" ++ messageName msg)) $ H.string (messageName msg)
            " (from "
            H.a ! HA.href (H.stringValue ("#state_" ++ messageFromState msg)) $ H.string (messageFromState msg)
            ")"
  where
    messagesFromHere = filter ((== stateName) . messageFromState) msgs
    messagesToHere = filter ((== stateName) . messageToState) msgs

renderMessage :: MessageDescription -> Html
renderMessage msg =
  H.div ! HA.class_ "message" $ do
    H.h3 ! HA.id (H.stringValue ("message_" ++ messageName msg)) $ H.string (messageName msg)
    H.p $ do
      H.a ! HA.href (H.stringValue ("#state_" ++ messageFromState msg)) $ H.string (messageFromState msg)
      "->"
      H.a ! HA.href (H.stringValue ("#state_" ++ messageToState msg)) $ H.string (messageToState msg)
    unless (null $ messagePayload msg) $ do
      H.p $ do
        "Payload: "
        forM_ (messagePayload msg) $ H.strong . H.string
    H.h4 "Serialization Format"
    fieldSpecToHTML (messageInfo msg)
  

renderProtocol :: ProtocolDescription -> [MessageDescription] -> Html
renderProtocol proto msgs =
  H.section ! HA.class_ "protocol" $ do
    H.h1 $ H.string (protocolName proto)
    H.section $ do
      H.h2 "States"
      mconcat <$> mapM (renderState msgs) (protocolStates proto)
    H.section $ do
      H.h2 "Messages"
      mconcat <$> mapM renderMessage msgs

fieldSpecToHTML :: FieldInfo -> Html
fieldSpecToHTML fi = do
  H.h5 ! HA.id (H.stringValue ("type_" ++ shortFieldType fi)) $ do
    H.string (shortFieldType fi)
  fromMaybe "" $ subfieldsToHTML (compoundField "" [("", fi)])

subfieldsToHTML :: FieldInfo -> Maybe Html
subfieldsToHTML (AliasField afi) =
  subfieldsToHTML (aliasFieldTarget afi)
subfieldsToHTML (CompoundField cfi) = Just $ do
  H.table $ do
    mapM_ subfieldToHtmlTR (compoundFieldSubfields cfi)
subfieldsToHTML _ = Nothing

fieldTypeToHtml :: FieldInfo -> Html
fieldTypeToHtml (AliasField info) = do
  H.string (aliasFieldName info)
  H.br
  fieldTypeToHtml (aliasFieldTarget info)
fieldTypeToHtml (ListField info) = do
  H.strong $ do
    "["
    H.string (shortFieldType (listElemInfo info))
    "]"
  H.br
  H.em $ "#items:"
  H.string $ formatFieldSize $ listSize info
  H.br
  H.em $ "item type:"
  fieldTypeToHtml (listElemInfo info)
  maybe "" (H.br <>) $
    subfieldsToHTML (listElemInfo info)
fieldTypeToHtml (ChoiceField info) = do
  H.em "Choose by:"
  case choiceCondition info of
    IndexField ref -> H.string ref
    IndexFlag ref mask -> H.string ref <> " & " <> H.string (printf "0x%04x" mask)
  H.table $ do
    H.tr $ do
      H.th "choice"
      H.th "size"
      H.th "type"
    sequence_ $
          [ H.tr $ do
              H.td $ H.string (show n)
              H.td ! HA.class_ "field-size" $ do
                H.string $ formatFieldSize (fieldSize optInfo)
              H.td $ do
                fieldTypeToHtml optInfo
                fromMaybe "" $ subfieldsToHTML optInfo
          | (n, optInfo) <- zip [0,1..] (choiceFieldAlternatives info)
          ]
fieldTypeToHtml (EnumField info) = do
  H.string (enumFieldType info)
  H.em " (enum)"
  H.table $ do
    H.tr $ do
      H.th "value"
      H.th "name"
    sequence_ $
        [ H.tr $ do
            H.td $ H.string (show val)
            H.td $ H.string name
        | (val, name) <- zip [0,1..] (enumFieldValues info)
        ]
fieldTypeToHtml fi =
  H.string . fieldType $ fi

subfieldToHtmlTR :: SubfieldInfo -> Html
subfieldToHtmlTR sfi =
  case subfieldsToHTML (subfieldInfo sfi) of
    Nothing -> do
      H.tr $ do
        H.th $ H.string (subfieldName sfi)
        H.td ! HA.class_ "field-size" $ H.string $ formatFieldSize (fieldSize (subfieldInfo sfi))
        H.td $ fieldTypeToHtml (subfieldInfo sfi)
    Just sfiHtml -> do
      H.tr $ do
        H.th ! HA.rowspan "2" $ H.string (subfieldName sfi)
        H.td ! HA.rowspan "2" ! HA.class_ "field-size" $ do
          H.string $ formatFieldSize (fieldSize (subfieldInfo sfi))
        H.td ! HA.colspan "2" $ fieldTypeToHtml (subfieldInfo sfi)
      H.tr $ do
        H.td ! HA.colspan "2" $ sfiHtml

wrapDoc :: Html -> Html
wrapDoc body = do
  H.docType
  H.html $ do
    H.head $ do
      H.style $ do
        "html { font-family: sans-serif; }"
        "div.state, div.message {"
        " background-color: #EEE;"
        " padding: 0.125rem 1rem; "
        " margin: 1rem; "
        "}"
        "table { "
        "border-collapse: collapse;"
        "margin-top: 0.25rem;"
        "margin-bottom: 0.25rem;"
        "}"
        "table td, table th {"
        "  border: solid 1px black;"
        "  text-align: left;"
        "  vertical-align: top;"
        "  padding: 0.25rem;"
        "  background-color: white; "
        "}"
        "table th {"
        "  background-color: #DDD"
        "}"
        ".field-size {"
        "  text-align: right;"
        "}"
    H.body body


main :: IO ()
main = do
  let stateInfos = $(describeProtocolStates ''ControlProtocol)
      messageInfos =
        [ $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'VersionMessage)
        , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'GenStagedKeyMessage)
        , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'QueryStagedKeyMessage)
        , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'DropStagedKeyMessage)
        , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'PublicKeyMessage)
        , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'InstallKeyMessage)
        , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'InstallResultMessage)
        , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'RequestInfoMessage)
        , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'InfoMessage)
        , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'AbortMessage)
        , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'EndMessage)
        , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'ProtocolErrorMessage)
        ]

  LText.writeFile "protocol.html" $ renderHtml . wrapDoc $ renderProtocol stateInfos messageInfos
