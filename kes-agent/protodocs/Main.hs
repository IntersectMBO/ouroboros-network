{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Documentation.Haddock.Types as Haddock
import qualified Documentation.Haddock.Parser as Haddock
import Debug.Trace

renderDescription :: Maybe Description -> Html
renderDescription Nothing = mempty
renderDescription (Just (Description h)) = do
  let (doc :: Haddock.DocH () String) = Haddock.toRegular . Haddock._doc . Haddock.parseParas Nothing $ unlines h
  renderHaddock doc

renderHaddock :: Haddock.DocH mod String -> Html
renderHaddock Haddock.DocEmpty = mempty
renderHaddock (Haddock.DocAppend a b) = renderHaddock a <> renderHaddock b
renderHaddock (Haddock.DocString str) = H.string str
renderHaddock (Haddock.DocParagraph a) = H.p (renderHaddock a)
renderHaddock (Haddock.DocIdentifier i) = H.span ! HA.class_ "identifier" $ H.string i
renderHaddock (Haddock.DocIdentifierUnchecked _) = H.span ! HA.class_ "unchecked" $ "**unchecked**"
renderHaddock (Haddock.DocModule (Haddock.ModLink label a)) = H.span ! HA.class_ "module" $ H.string label
renderHaddock (Haddock.DocWarning a) = H.div ! HA.class_ "warning" $ renderHaddock a
renderHaddock (Haddock.DocEmphasis a) = H.em $ renderHaddock a
renderHaddock (Haddock.DocMonospaced a) = H.code $ renderHaddock a
renderHaddock (Haddock.DocBold a) = H.strong $ renderHaddock a
renderHaddock (Haddock.DocUnorderedList items) = H.ul $ forM_ items $ \item -> H.li (renderHaddock item)
renderHaddock (Haddock.DocOrderedList items) = H.ol $ forM_ items $ \(_, item) -> H.li (renderHaddock item)
renderHaddock (Haddock.DocDefList items) =
  H.dl $ forM_ items $ \(title, body) ->
    H.div $ do
      H.dt $ renderHaddock title
      H.dd $ renderHaddock body
renderHaddock (Haddock.DocCodeBlock a) = H.code $ renderHaddock a
renderHaddock (Haddock.DocHyperlink (Haddock.Hyperlink url a)) =
  H.a ! HA.href (H.stringValue url) $ maybe (H.string url) renderHaddock a
renderHaddock (Haddock.DocPic (Haddock.Picture url title)) =
  H.div ! HA.class_ "omitted" $ do
    forM_ title (H.p . H.string)
    H.p . H.string $ url
renderHaddock (Haddock.DocMathInline str) = H.span ! HA.class_ "math" $ H.string str
renderHaddock (Haddock.DocMathDisplay str) = H.div ! HA.class_ "math" $ H.string str
renderHaddock (Haddock.DocAName str) = H.span ! HA.class_ "aname" $ H.string str
renderHaddock (Haddock.DocProperty str) = H.span ! HA.class_ "property" $ H.string str
renderHaddock (Haddock.DocExamples examples) =
  forM_ examples $ \(Haddock.Example expr results) -> do
    H.div ! HA.class_ "example" $ do
      H.code ! HA.class_ "expr" $ H.string expr
      H.code ! HA.class_ "result" $ do
        forM_ results $ \resultLine -> do
          H.string resultLine
          H.br
renderHaddock (Haddock.DocHeader (Haddock.Header level a)) = do
  let h = case level of
            1 -> H.h1
            2 -> H.h2
            3 -> H.h3
            4 -> H.h4
            5 -> H.h5
            _ -> H.h6
  h $ renderHaddock a
renderHaddock (Haddock.DocTable (Haddock.Table headerRows bodyRows)) = do
  H.table $ do
    H.thead $ do
      forM_ headerRows $ \row -> do
        H.tr $ do
          forM_ (Haddock.tableRowCells row) $ \(Haddock.TableCell colspan rowspan body) -> do
            H.th ! HA.colspan (H.toValue colspan)
                 ! HA.rowspan (H.toValue rowspan)
                 $ renderHaddock body
    H.tbody $ do
      forM_ bodyRows $ \row -> do
        H.tr $ do
          forM_ (Haddock.tableRowCells row) $ \(Haddock.TableCell colspan rowspan body) -> do
            H.td ! HA.colspan (H.toValue colspan)
                 ! HA.rowspan (H.toValue rowspan)
                 $ renderHaddock body

renderState :: [MessageDescription] -> (String, Maybe Description) -> Html
renderState msgs (stateName, descriptionMay) =
  H.div ! HA.class_ "state" $ do
    H.h3 ! HA.id (H.stringValue ("state_" ++ stateName)) $ H.string stateName
    renderDescription descriptionMay
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
    renderDescription (messageDescription msg)
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
    renderDescription (protocolDescription proto)
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
  let protocolInfo = $(describeProtocol ''ControlProtocol)
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

  LText.writeFile "protocol.html" $ renderHtml . wrapDoc $ renderProtocol protocolInfo messageInfos
