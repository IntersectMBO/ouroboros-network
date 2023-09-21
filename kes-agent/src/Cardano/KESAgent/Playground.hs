{-# LANGUAGE TypeApplications #-}

module Cardano.KESAgent.Playground
where

import Data.Proxy
import Cardano.KESAgent.Serialization.Spec
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.Protocols.Control.Protocol
import Cardano.KESAgent.Protocols.Control.Driver
import GHC.Generics
import Cardano.Crypto.KES.Class
import Text.Printf

htmlencode :: String -> String
htmlencode = concatMap htmlencodeChar

htmlencodeChar :: Char -> String
htmlencodeChar '&' = "&amp;"
htmlencodeChar '<' = "&lt;"
htmlencodeChar '>' = "&gt;"
htmlencodeChar '"' = "&quot;"
htmlencodeChar '\'' = "&apos;"
htmlencodeChar c = [c]

specToHTML :: FieldInfo -> String
specToHTML =
  wrapHTML . fieldSpecToHTML

specsToHTML :: [FieldInfo] -> String
specsToHTML =
  wrapHTML . concatMap fieldSpecToHTML

wrapHTML :: String -> String
wrapHTML inner =
  unlines
    [ "<!DOCTYPE html>"
    , "<html>"
    , "<head>"
    , "<style type='text/css'>"
    , "table { border-collapse: collapse; }"
    , "table td, table th {"
    , "  border: solid 1px black;"
    , "  text-align: left;"
    , "  vertical-align: top;"
    , "  padding: 0.25em;"
    , "}"
    , "table th {"
    , "  background-color: #DDD"
    , "}"
    , ".field-size {"
    , "  text-align: right;"
    , "}"
    , "</style>"
    , "</head>"
    , "<body>"
    , inner
    , "</body>"
    , "</html>"
    ]

fieldSpecToHTML :: FieldInfo -> String
fieldSpecToHTML fi =
  mconcat
    [ "<h3 id='", htmlencode (shortFieldType fi), "'>"
    , htmlencode (fieldType fi)
    , "</h3>"
    , subfieldsToHTML (compoundField "" [("", fi)])
    ]

subfieldsToHTML :: FieldInfo -> String
subfieldsToHTML (AliasField afi) =
  subfieldsToHTML (aliasFieldTarget afi)
subfieldsToHTML (CompoundField cfi) =
  mconcat
    [ "<table>"
    , concatMap subfieldToHtmlTR (compoundFieldSubfields cfi)
    , "</table>"
    ]
subfieldsToHTML _ = ""

fieldTypeToHtml :: FieldInfo -> String
fieldTypeToHtml (AliasField info) =
  htmlencode (aliasFieldName info) ++
  "<br/>= " ++
  fieldTypeToHtml (aliasFieldTarget info)
fieldTypeToHtml (ListField info) =
  mconcat $
    [ "<strong>["
    , htmlencode (shortFieldType (listElemInfo info))
    , "]</strong><br/>"
    , "<em>#items: </em>"
    , htmlencode (formatFieldSize $ listSize info)
    , "<br/>"
    , "<em>item type: </em>"
    , fieldTypeToHtml (listElemInfo info)
    , "<br/>"
    , subfieldsToHTML (listElemInfo info)
    ]
fieldTypeToHtml (ChoiceField info) =
  mconcat $
    [ "<em>Choose by: </em>"
    , case choiceCondition info of
        IndexField ref -> htmlencode ref
        IndexFlag ref mask -> htmlencode ref <> " &amp; " <> htmlencode (printf "0x%04x" mask)
    , "<table>"
    , "<tr><th>choice</th><th>size</th><th>type</th></tr>"
    ] ++
    [ "<tr>" <>
      "<td>" <>
      htmlencode (show n) <>
      "</td>" <>
      "<td class='field-size'>" <>
      htmlencode (formatFieldSize (fieldSize optInfo)) <>
      "</td>" <>
      "<td>" <>
      fieldTypeToHtml optInfo <>
      subfieldsToHTML optInfo <>
      "</td>" <>
      "</tr>"
    | (n, optInfo) <- zip [0,1..] (choiceFieldAlternatives info)
    ] ++
    [ "</table>"
    ]
fieldTypeToHtml (EnumField info) =
  mconcat $
    [ htmlencode (enumFieldType info)
    , "<em> (enum)</em>"
    , "<table>"
    , "<tr><th>value</th><th>name</th></tr>"
    ] ++
    [ "<tr>" <>
      "<td>" <>
      htmlencode (show val) <>
      "</td>" <>
      "<td>" <>
      htmlencode name <>
      "</td>" <>
      "</tr>"
    | (val, name) <- zip [0,1..] (enumFieldValues info)
    ] ++
    [ "</table>"
    ]
fieldTypeToHtml fi =
  htmlencode . fieldType $ fi

subfieldToHtmlTR :: SubfieldInfo -> String
subfieldToHtmlTR sfi =
  case subfieldsToHTML (subfieldInfo sfi) of
    "" ->
      mconcat
        [ "<tr>"
        , "<th>"
        , subfieldName sfi
        , "</th>"
        , "<td class='field-size'>", htmlencode $ formatFieldSize (fieldSize (subfieldInfo sfi)), "</td>"
        , "<td>", fieldTypeToHtml (subfieldInfo sfi), "</td>"
        , "</tr>"
        ]
    sfiHtml ->
      mconcat
        [ "<tr>"
        , "<th rowspan=2>"
        , subfieldName sfi
        , "</th>"
        , "<td rowspan=2 class='field-size'>", htmlencode $ formatFieldSize (fieldSize (subfieldInfo sfi)), "</td>"
        , "<td colspan=2>", fieldTypeToHtml (subfieldInfo sfi), "</td>"
        , "</tr>"
        , "<tr>"
        , "<td colspan=2>", subfieldsToHTML (subfieldInfo sfi), "</td>"
        , "</tr>"
        ]

allInfos =
  [ info @(AgentInfo StandardCrypto) Proxy
  , info @(BundleInfo StandardCrypto) Proxy
  ]
  

runPlayground :: IO ()
runPlayground = do
  writeFile "spec.html" (specsToHTML allInfos)
