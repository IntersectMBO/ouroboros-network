{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main
where

import qualified Cardano.KESAgent.Protocols.Control.Driver ()
import Cardano.KESAgent.Protocols.Control.Protocol (ControlProtocol)
import qualified Cardano.KESAgent.Protocols.Control.Protocol as Control
import qualified Cardano.KESAgent.Protocols.Service.Driver ()
import Cardano.KESAgent.Protocols.Service.Protocol (ServiceProtocol)
import qualified Cardano.KESAgent.Protocols.Service.Protocol as Service
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Protodocs.TH
import Cardano.KESAgent.Serialization.Spec
import Cardano.KESAgent.Serialization.Spec.Class
import Data.List

import Control.Monad
import Data.Maybe
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.IO as LText
import Debug.Trace
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import Language.Haskell.TH
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Printf

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

data TOC a = TOC a [TOC a]
  deriving (Show, Eq, Ord)

renderTOC :: TOC (String, String) -> Html
renderTOC (TOC (label, href) children) = do
  H.section ! HA.class_ "toc" $ do
    H.a ! HA.href (H.stringValue $ "#" ++ href) $ H.string label
    forM_ children renderTOC

stateID :: String -> String -> String
stateID protocolName stateName = protocolName ++ "_state_" ++ stateName

stateTOC :: String -> String -> TOC (String, String)
stateTOC protocolName stateName = TOC (stateName, stateID protocolName stateName) []

renderState :: String -> [MessageDescription] -> (String, Maybe Description) -> Html
renderState protocolName msgs (stateName, descriptionMay) =
  H.div ! HA.class_ "state" $ do
    H.h3 ! HA.id (H.stringValue (stateID protocolName stateName)) $ H.string stateName
    renderDescription descriptionMay
    unless (null messagesFromHere) $ do
      H.h4 "Messages from here:"
      H.ul $ do
        forM_ messagesFromHere $ \msg -> do
          H.li $ do
            H.strong $
              H.a ! HA.href (H.stringValue ("#" ++ messageID protocolName (messageName msg))) $ H.string (messageName msg)
            " (to "
            H.a ! HA.href (H.stringValue ("#" ++ stateID protocolName (messageToState msg))) $ H.string (messageToState msg)
            ")"
    unless (null messagesToHere) $ do
      H.h4 "Messages to here:"
      H.ul $ do
        forM_ messagesToHere $ \msg -> do
          H.li $ do
            H.strong $
              H.a ! HA.href (H.stringValue ("#" ++ messageID protocolName (messageName msg))) $ H.string (messageName msg)
            " (from "
            H.a ! HA.href (H.stringValue ("#" ++ stateID protocolName (messageFromState msg))) $ H.string (messageFromState msg)
            ")"
  where
    messagesFromHere = filter ((== stateName) . messageFromState) msgs
    messagesToHere = filter ((== stateName) . messageToState) msgs

messageID :: String -> String -> String
messageID protocolName messageName = protocolName ++ "_message_" ++ messageName

messageTOC :: String -> MessageDescription -> TOC (String, String)
messageTOC protocolName msg =
  TOC (messageName msg, messageID protocolName (messageName msg)) []

renderMessage :: String -> MessageDescription -> Html
renderMessage protocolName msg =
  H.div ! HA.class_ "message" $ do
    H.h3 ! HA.id (H.stringValue (messageID protocolName (messageName msg))) $ H.string (messageName msg)
    renderDescription (messageDescription msg)
    H.h4 $ "State Transition"
    H.p $ do
      H.a ! HA.href (H.stringValue ("#" ++ stateID protocolName (messageFromState msg))) $ H.string (messageFromState msg)
      " -> "
      H.a ! HA.href (H.stringValue ("#" ++ stateID protocolName (messageToState msg))) $ H.string (messageToState msg)
    unless (null $ messagePayload msg) $ do
      H.h4 "Payload"
      H.ul $ do
        forM_ (messagePayload msg) $ H.li . H.string
    H.h4 "Serialization Format"
    fieldSpecToHTML (messageInfo msg)
  
protocolTOC :: ProtocolDescription -> [MessageDescription] -> TOC (String, String)
protocolTOC proto msgs =
  let protoName = protocolName proto
  in
    TOC (protoName, protoName)
      [ TOC ("States", protoName ++ "_states")
        [ stateTOC protoName stateName | (stateName, _) <- protocolStates proto ]
      , TOC ("Messages", protoName ++ "_messages")
        [ messageTOC protoName msg | msg <- msgs ]
      ]

renderProtocol :: ProtocolDescription -> [MessageDescription] -> Html
renderProtocol proto msgs = do
  let protoName = protocolName proto
  H.section ! HA.class_ "protocol" $ do
    H.h1 ! HA.id (H.stringValue protoName) $ H.string protoName
    "Version ID: "
    H.code $ H.text (decodeUtf8 . unVersionIdentifier $ protocolIdentifier proto)
    renderDescription (protocolDescription proto)
    H.section $ do
      H.h2 ! HA.id (H.stringValue $ protoName ++ "_states") $ "States"
      mconcat <$> mapM (renderState protoName msgs) (protocolStates proto)
    H.section $ do
      H.h2 ! HA.id (H.stringValue $ protoName ++ "_messages") $ "Messages"
      mconcat <$> mapM (renderMessage protoName) msgs

fieldSpecToHTML :: FieldInfo -> Html
fieldSpecToHTML fi = do
  forM_ (fieldSpecAnnotations fi) (H.p . H.string)
  fromMaybe "" $ subfieldsToHTML (compoundField "" [("", fi)])

fieldSpecAnnotations :: FieldInfo -> [String]
fieldSpecAnnotations (AnnField ann fi) =
  ann : fieldSpecAnnotations fi
fieldSpecAnnotations _ = []

subfieldsToHTML :: FieldInfo -> Maybe Html
subfieldsToHTML (AnnField _ fi) =
  subfieldsToHTML fi
subfieldsToHTML (AliasField afi) =
  subfieldsToHTML (aliasFieldTarget afi)
subfieldsToHTML (CompoundField cfi) = Just $ do
  H.table $ do
    mapM_ subfieldToHtmlTR (compoundFieldSubfields cfi)
subfieldsToHTML _ = Nothing

fieldTypeToHtml :: FieldInfo -> Html
fieldTypeToHtml (AnnField _ fi) =
  fieldTypeToHtml fi
fieldTypeToHtml (AliasField info) = do
  H.strong $ H.string (aliasFieldName info)
  H.br
  H.em "This type is an alias for: "
  fieldTypeToHtml (aliasFieldTarget info)
fieldTypeToHtml (ListField info) = do
  H.strong $ do
    "["
    H.string (shortFieldType (listElemInfo info))
    "]"
  H.br
  H.em $ "#items: "
  H.string $ formatFieldSize $ listSize info
  H.br
  H.em $ "item type: "
  fieldTypeToHtml (listElemInfo info)
  maybe "" (H.br <>) $
    subfieldsToHTML (listElemInfo info)
fieldTypeToHtml (ChoiceField info) = do
  H.em "Choice"
  let choiceLabel = case choiceCondition info of
        IndexField ref -> H.string ref
        IndexFlag ref mask -> H.string ref <> " & " <> H.string (printf "0x%04x" mask)
  H.table $ do
    H.tr $ do
      H.th choiceLabel
      H.th "size"
      H.th "type"
    sequence_ $
          [ H.tr $ do
              H.td ! HA.class_ "choice-value" $ H.string (show n)
              H.td ! HA.class_ "field-size" $ do
                H.string $ formatFieldSize (fieldSize optInfo)
              H.td $ do
                fieldTypeToHtml optInfo
                fromMaybe "" $ subfieldsToHTML optInfo
          | (n, optInfo) <- zip [0,1..] (choiceFieldAlternatives info)
          ]
fieldTypeToHtml (EnumField info) = do
  H.strong $ H.string (enumFieldType info)
  H.em " (enum)"
  H.table $ do
    H.tr $ do
      H.th "value"
      H.th "name"
    sequence_ $
        [ H.tr $ do
            H.td ! HA.class_ "enum-value" $ H.string (show val)
            H.td $ H.string name
        | (val, name) <- enumFieldValues info
        ]
fieldTypeToHtml fi =
  H.strong . H.string . fieldType $ fi

subfieldToHtmlTR :: SubfieldInfo -> Html
subfieldToHtmlTR sfi =
  case subfieldsToHTML (subfieldInfo sfi) of
    Nothing -> do
      H.tr $ do
        H.th ! HA.colspan "2" $ H.string (subfieldName sfi)
      H.tr $ do
        H.td ! HA.class_ "field-size" $ H.string $ formatFieldSize (fieldSize (subfieldInfo sfi))
        H.td $ fieldTypeToHtml (subfieldInfo sfi)
    Just sfiHtml -> do
      H.tr $ do
        H.th ! HA.colspan "2" $ H.string (subfieldName sfi)
      H.tr $ do
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
        "body { max-width: 60rem; margin-left: auto; margin-right: auto; padding: 1rem; }"
        "h1 { font-size: 3rem; }"
        "h2 { font-size: 2rem; }"
        "h3 { font-size: 1.5rem; }"
        "h4 { font-size: 1.25rem; }"
        "h5 { font-size: 1.1rem; }"
        "h6 { font-size: 1rem; }"
        "div.state, div.message {"
        " background-color: #EEE;"
        " padding: 0.125rem 1rem; "
        " margin: 1rem 0 1rem 0;"
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
        ".choice-value,"
        ".enum-value,"
        ".field-size {"
        "  text-align: right;"
        "  width: 4rem;"
        "}"
        ".toc>.toc {"
        "padding-left: 2rem;"
        "}"
    H.body body


main :: IO ()
main = do
  LText.writeFile "protocol.html" $ renderHtml . wrapDoc $ tocHtml <> serviceProtoHtml <> controlProtoHtml
  where
    serviceInfo =
      $(describeProtocol ''ServiceProtocol ''StandardCrypto)
    serviceMInfos =
      [ $(describeProtocolMessage ''ServiceProtocol ''StandardCrypto 'Service.VersionMessage)
      , $(describeProtocolMessage ''ServiceProtocol ''StandardCrypto 'Service.KeyMessage)
      , $(describeProtocolMessage ''ServiceProtocol ''StandardCrypto 'Service.AbortMessage)
      , $(describeProtocolMessage ''ServiceProtocol ''StandardCrypto 'Service.ServerDisconnectMessage)
      , $(describeProtocolMessage ''ServiceProtocol ''StandardCrypto 'Service.ClientDisconnectMessage)
      , $(describeProtocolMessage ''ServiceProtocol ''StandardCrypto 'Service.ProtocolErrorMessage)
      ]
    controlInfo =
      $(describeProtocol ''ControlProtocol ''StandardCrypto)
    controlMInfos =
      [ $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'Control.VersionMessage)
      , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'Control.GenStagedKeyMessage)
      , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'Control.QueryStagedKeyMessage)
      , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'Control.DropStagedKeyMessage)
      , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'Control.PublicKeyMessage)
      , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'Control.InstallKeyMessage)
      , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'Control.InstallResultMessage)
      , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'Control.RequestInfoMessage)
      , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'Control.InfoMessage)
      , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'Control.AbortMessage)
      , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'Control.EndMessage)
      , $(describeProtocolMessage ''ControlProtocol ''StandardCrypto 'Control.ProtocolErrorMessage)
      ]

    serviceProtoHtml = renderProtocol serviceInfo serviceMInfos
    controlProtoHtml = renderProtocol controlInfo controlMInfos
    tocHtml = H.div ! HA.class_ "toc-master" $ do
                H.h1 "Table Of Contents"
                renderTOC $ TOC ("Protocols", "")
                  [ protocolTOC serviceInfo serviceMInfos
                  , protocolTOC controlInfo controlMInfos
                  ]
